use serde::{Deserialize, Serialize};
use std::collections::HashMap;
use std::env;
use std::fs;
use std::io::{self, Write};
use std::path::{Path, PathBuf};
use std::time::{SystemTime, UNIX_EPOCH};

#[derive(Debug, Serialize, Deserialize, Clone)]
struct StatsEntry {
    path: String,
    count: u64,
    #[serde(rename = "lastUsed")]
    last_used: u64,
}

fn now_secs() -> u64 {
    SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .unwrap()
        .as_secs()
}

fn detect_project_type(dir: &Path) -> &'static str {
    if dir.join("Cargo.toml").exists() {
        "rust"
    } else if dir.join("package.json").exists() {
        "nodejs"
    } else if dir.join("pyproject.toml").exists()
        || dir.join("setup.py").exists()
        || dir.join("requirements.txt").exists()
    {
        "python"
    } else if dir.join("go.mod").exists() {
        "go"
    } else if dir.join(".homesick").is_dir() {
        "dotfiles"
    } else if dir.join("Gemfile").exists() {
        "ruby"
    } else if dir.join("pom.xml").exists() || dir.join("build.gradle").exists() {
        "java"
    } else {
        "general"
    }
}

fn project_type_icon(pt: &str) -> &'static str {
    match pt {
        "rust" => "🦀",
        "nodejs" => "📦",
        "python" => "🐍",
        "go" => "🐹",
        "dotfiles" => "🏠",
        "ruby" => "💎",
        "java" => "☕",
        _ => "💻",
    }
}

fn calculate_frecency(count: u64, last_used: u64, now: u64) -> f64 {
    let days_diff = if now > last_used {
        (now - last_used) / 86400
    } else {
        0
    };
    let recency_weight = if days_diff <= 1 {
        1.0
    } else if days_diff <= 7 {
        0.8
    } else if days_diff <= 30 {
        0.5
    } else {
        0.2
    };
    count as f64 * 0.5 + recency_weight * 50.0 * 0.5
}

fn format_last_used(last_used: u64, now: u64) -> String {
    let diff = if now > last_used {
        now - last_used
    } else {
        0
    };
    if diff < 3600 {
        format!("{}m ago", diff / 60)
    } else if diff < 86400 {
        format!("{}h ago", diff / 3600)
    } else if diff < 604800 {
        format!("{}d ago", diff / 86400)
    } else {
        format!("{}w ago", diff / 604800)
    }
}

fn load_stats(path: &Path) -> HashMap<String, StatsEntry> {
    fs::read_to_string(path)
        .ok()
        .and_then(|s| serde_json::from_str(&s).ok())
        .unwrap_or_default()
}

fn save_stats(path: &Path, stats: &HashMap<String, StatsEntry>) {
    if let Some(parent) = path.parent() {
        let _ = fs::create_dir_all(parent);
    }
    if let Ok(json) = serde_json::to_string_pretty(stats) {
        let _ = fs::write(path, json);
    }
}

struct ScanConfig {
    search_paths: Vec<PathBuf>,
    max_depth: u32,
    enable_stats: bool,
    stats_file: PathBuf,
    include_home_toplevel: bool,
}

struct Entry {
    path: PathBuf,
    frecency: f64,
    count: u64,
    time_str: String,
    project_type: &'static str,
}

fn scan_directories(config: &ScanConfig) -> Vec<PathBuf> {
    let mut dirs = Vec::new();

    for search_path in &config.search_paths {
        if !search_path.is_dir() {
            continue;
        }
        // Add the search path itself
        dirs.push(search_path.clone());

        // Add children up to max_depth
        if config.max_depth >= 1 {
            scan_children(search_path, 1, config.max_depth, &mut dirs);
        }
    }

    // Add home top-level directories if enabled
    if config.include_home_toplevel {
        if let Some(home) = dirs::home_dir_get() {
            if let Ok(entries) = fs::read_dir(&home) {
                for entry in entries.flatten() {
                    let path = entry.path();
                    if !path.is_dir() {
                        continue;
                    }
                    // Skip hidden dirs
                    if entry
                        .file_name()
                        .to_str()
                        .is_some_and(|n| n.starts_with('.'))
                    {
                        continue;
                    }
                    // Skip if already covered by search paths
                    let dominated = config.search_paths.iter().any(|sp| {
                        path == *sp || path.starts_with(sp)
                    });
                    if !dominated {
                        dirs.push(path);
                    }
                }
            }
        }
    }

    dirs
}

fn scan_children(dir: &Path, current_depth: u32, max_depth: u32, out: &mut Vec<PathBuf>) {
    let entries = match fs::read_dir(dir) {
        Ok(e) => e,
        Err(_) => return,
    };
    for entry in entries.flatten() {
        let path = entry.path();
        if path.is_dir() {
            out.push(path.clone());
            if current_depth < max_depth {
                scan_children(&path, current_depth + 1, max_depth, out);
            }
        }
    }
}

// Simple home dir helper to avoid adding a dependency
mod dirs {
    use std::path::PathBuf;
    pub fn home_dir_get() -> Option<PathBuf> {
        std::env::var_os("HOME").map(PathBuf::from)
    }
}

fn run_scan(config: &ScanConfig) {
    let now = now_secs();
    let stats = if config.enable_stats {
        load_stats(&config.stats_file)
    } else {
        HashMap::new()
    };

    let directories = scan_directories(config);

    let mut entries: Vec<Entry> = directories
        .into_iter()
        .map(|path| {
            let project_type = detect_project_type(&path);
            let basename = path
                .file_name()
                .and_then(|n| n.to_str())
                .unwrap_or("");

            let (frecency, count, time_str) = if config.enable_stats {
                if let Some(se) = stats.get(basename) {
                    let f = calculate_frecency(se.count, se.last_used, now);
                    let t = format_last_used(se.last_used, now);
                    (f, se.count, t)
                } else {
                    (0.0, 0, "never".to_string())
                }
            } else {
                (0.0, 0, String::new())
            };

            Entry {
                path,
                frecency,
                count,
                time_str,
                project_type,
            }
        })
        .collect();

    if config.enable_stats {
        entries.sort_by(|a, b| b.frecency.partial_cmp(&a.frecency).unwrap());
    }

    let stdout = io::stdout();
    let mut out = io::BufWriter::new(stdout.lock());

    for e in &entries {
        let dir_str = e.path.to_string_lossy();
        let icon = project_type_icon(e.project_type);

        if config.enable_stats {
            let star = if e.frecency > 20.0 { "⭐ " } else { "" };
            // Format: [star][score]  [type] path  (time, countx)\tpath
            write!(out, "{}{:<6.0}  {} {}", star, e.frecency, icon, dir_str).unwrap();
            if e.count > 0 {
                write!(out, "  \x1b[90m({}, {}x)\x1b[0m", e.time_str, e.count).unwrap();
            }
            write!(out, "\t{}\n", dir_str).unwrap();
        } else {
            // No stats: [type] path\tpath
            write!(out, " {} {}\t{}\n", icon, dir_str, dir_str).unwrap();
        }
    }
}

fn run_track(stats_file: &Path, session_path: &str) {
    let mut stats = load_stats(stats_file);
    let now = now_secs();
    let key = Path::new(session_path)
        .file_name()
        .and_then(|n| n.to_str())
        .unwrap_or(session_path)
        .to_string();

    let entry = stats.entry(key).or_insert(StatsEntry {
        path: session_path.to_string(),
        count: 0,
        last_used: now,
    });
    entry.count += 1;
    entry.last_used = now;
    entry.path = session_path.to_string();

    save_stats(stats_file, &stats);
}

fn main() {
    let args: Vec<String> = env::args().collect();

    // Parse --track mode
    if args.len() >= 2 && args[1] == "--track" {
        let stats_file = args
            .iter()
            .position(|a| a == "--stats-file")
            .and_then(|i| args.get(i + 1))
            .map(PathBuf::from)
            .unwrap_or_else(|| {
                let home = env::var("HOME").unwrap_or_default();
                PathBuf::from(home).join(".config/tmux-sessionizer/stats.json")
            });
        let path = args.get(2).map(|s| s.as_str()).unwrap_or("");
        if path.is_empty() || path.starts_with("--") {
            eprintln!("Usage: tmux-sessionizer-scan --track <path> [--stats-file <file>]");
            std::process::exit(1);
        }
        run_track(&stats_file, path);
        return;
    }

    // Parse scan mode args
    let mut config = ScanConfig {
        search_paths: Vec::new(),
        max_depth: 1,
        enable_stats: false,
        stats_file: PathBuf::from(
            env::var("HOME").unwrap_or_default()
                + "/.config/tmux-sessionizer/stats.json",
        ),
        include_home_toplevel: false,
    };

    let mut i = 1;
    while i < args.len() {
        match args[i].as_str() {
            "--search-paths" => {
                i += 1;
                while i < args.len() && !args[i].starts_with("--") {
                    config.search_paths.push(PathBuf::from(&args[i]));
                    i += 1;
                }
                continue; // skip the i += 1 at bottom
            }
            "--max-depth" => {
                i += 1;
                if let Some(v) = args.get(i) {
                    config.max_depth = v.parse().unwrap_or(1);
                }
            }
            "--enable-stats" => {
                config.enable_stats = true;
            }
            "--stats-file" => {
                i += 1;
                if let Some(v) = args.get(i) {
                    config.stats_file = PathBuf::from(v);
                }
            }
            "--include-home-toplevel" => {
                config.include_home_toplevel = true;
            }
            _ => {}
        }
        i += 1;
    }

    run_scan(&config);
}
