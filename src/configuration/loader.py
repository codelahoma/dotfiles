"""Configuration loading system with hierarchical override support.

This module implements the FlowLoom configuration hierarchy:
1. Embedded defaults
2. flowloom.json (project-level)
3. Environment variables (FLOWLOOM_*)
4. Command-line arguments
"""

import json
import os
from pathlib import Path
from typing import Dict, Any, Optional, List, Union
from dataclasses import dataclass, field

from .models import FlowLoomConfig
from .schema import ConfigurationValidator, ValidationResult


@dataclass
class ConfigurationSource:
    """Represents a configuration source with metadata."""
    name: str
    path: Optional[Path] = None
    data: Dict[str, Any] = field(default_factory=dict)
    loaded: bool = False
    error: Optional[str] = None


@dataclass 
class LoadResult:
    """Result of configuration loading operation."""
    config: Optional[FlowLoomConfig] = None
    sources: List[ConfigurationSource] = field(default_factory=list)
    validation: Optional[ValidationResult] = None
    warnings: List[str] = field(default_factory=list)
    errors: List[str] = field(default_factory=list)


class EnvironmentVariableResolver:
    """Resolves environment variables in configuration values."""
    
    def resolve(self, data: Dict[str, Any]) -> Dict[str, Any]:
        """Recursively resolve environment variables in configuration data."""
        return self._resolve_recursive(data)
    
    def _resolve_recursive(self, obj: Any) -> Any:
        """Recursively process configuration values."""
        if isinstance(obj, dict):
            return {key: self._resolve_recursive(value) for key, value in obj.items()}
        elif isinstance(obj, list):
            return [self._resolve_recursive(item) for item in obj]
        elif isinstance(obj, str):
            return self._resolve_string(obj)
        else:
            return obj
    
    def _resolve_string(self, value: str) -> str:
        """Resolve environment variables in string values."""
        import re
        
        # Pattern: ${VAR_NAME:default_value} or ${VAR_NAME}
        pattern = r'\$\{([^}:]+)(?::([^}]*))?\}'
        
        def replace_var(match):
            var_name = match.group(1)
            default_value = match.group(2) if match.group(2) is not None else ""
            return os.getenv(var_name, default_value)
        
        return re.sub(pattern, replace_var, value)


class ConfigurationLoader:
    """Loads FlowLoom configuration with hierarchical override support."""
    
    def __init__(self, project_root: Optional[Path] = None):
        self.project_root = Path(project_root) if project_root else Path.cwd()
        self.validator = ConfigurationValidator()
        self.env_resolver = EnvironmentVariableResolver()
        self._defaults = self._get_embedded_defaults()
    
    def load_configuration(
        self, 
        config_file: Optional[Path] = None,
        env_prefix: str = "FLOWLOOM_",
        cli_args: Optional[Dict[str, Any]] = None
    ) -> LoadResult:
        """Load configuration with hierarchical override support."""
        result = LoadResult()
        
        try:
            # 1. Load embedded defaults
            defaults_source = self._load_defaults()
            result.sources.append(defaults_source)
            merged_config = defaults_source.data.copy()
            
            # 2. Load project configuration file
            project_source = self._load_project_config(config_file)
            result.sources.append(project_source)
            if project_source.loaded:
                merged_config = self._deep_merge(merged_config, project_source.data)
            
            # 3. Apply environment variables
            env_source = self._load_environment_config(env_prefix)
            result.sources.append(env_source)
            if env_source.loaded:
                merged_config = self._deep_merge(merged_config, env_source.data)
            
            # 4. Apply command-line arguments
            if cli_args:
                cli_source = ConfigurationSource(
                    name="command_line",
                    data=cli_args,
                    loaded=True
                )
                result.sources.append(cli_source)
                merged_config = self._deep_merge(merged_config, cli_args)
            
            # 5. Resolve environment variables in final config
            resolved_config = self.env_resolver.resolve(merged_config)
            
            # 6. Validate merged configuration
            validation_result = self.validator.validate_config(resolved_config)
            result.validation = validation_result
            
            if validation_result.valid:
                # 7. Create FlowLoom configuration object
                try:
                    result.config = FlowLoomConfig(**resolved_config)
                except Exception as e:
                    result.errors.append(f"Failed to create configuration object: {e}")
            else:
                result.errors.extend(validation_result.errors)
            
            # Collect warnings
            if validation_result.warnings:
                result.warnings.extend(validation_result.warnings)
            
            # Add source-specific warnings
            for source in result.sources:
                if source.error:
                    result.warnings.append(f"{source.name}: {source.error}")
            
        except Exception as e:
            result.errors.append(f"Configuration loading failed: {e}")
        
        return result
    
    def _load_defaults(self) -> ConfigurationSource:
        """Load embedded default configuration."""
        return ConfigurationSource(
            name="embedded_defaults",
            data=self._defaults,
            loaded=True
        )
    
    def _load_project_config(self, config_file: Optional[Path] = None) -> ConfigurationSource:
        """Load project-level configuration from flowloom.json."""
        if config_file:
            config_path = config_file
        else:
            config_path = self.project_root / "flowloom.json"
        
        source = ConfigurationSource(
            name="project_config",
            path=config_path
        )
        
        if not config_path.exists():
            source.error = f"Configuration file not found: {config_path}"
            return source
        
        try:
            with open(config_path) as f:
                source.data = json.load(f)
            source.loaded = True
        except json.JSONDecodeError as e:
            source.error = f"Invalid JSON in {config_path}: {e}"
        except Exception as e:
            source.error = f"Error reading {config_path}: {e}"
        
        return source
    
    def _load_environment_config(self, prefix: str) -> ConfigurationSource:
        """Load configuration from environment variables."""
        source = ConfigurationSource(name="environment_variables")
        
        env_config = {}
        prefix_len = len(prefix)
        
        for key, value in os.environ.items():
            if key.startswith(prefix):
                # Convert FLOWLOOM_AGENTS_MAX_CONCURRENT to agents.maxConcurrent
                config_key = key[prefix_len:].lower()
                config_path = config_key.split('_')
                
                # Convert to camelCase for nested keys
                if len(config_path) > 1:
                    config_path = [config_path[0]] + [
                        self._to_camel_case(part) for part in config_path[1:]
                    ]
                
                # Set nested value
                self._set_nested_value(env_config, config_path, self._parse_env_value(value))
        
        if env_config:
            source.data = env_config
            source.loaded = True
        
        return source
    
    def _get_embedded_defaults(self) -> Dict[str, Any]:
        """Get embedded default configuration."""
        return {
            "version": "1.0",
            "project": {
                "name": "FlowLoom Project",
                "type": "ai-coordination-platform"
            },
            "agents": {
                "maxConcurrent": 3,
                "coordinationStrategy": "hierarchical",
                "defaultModel": "claude-sonnet-4",
                "memorySystem": "dual",
                "agentTimeout": "30m",
                "crossAgentSharing": True
            },
            "resources": {
                "rateLimiting": {
                    "globalRequestsPerMinute": 100,
                    "perAgentRequestsPerMinute": 30,
                    "burstAllowance": 10
                },
                "memory": {
                    "retentionPeriod": "7d",
                    "maxContextWindow": "200k",
                    "contextStrategy": "truncate-oldest"
                },
                "retryPolicy": {
                    "maxRetries": 3,
                    "backoffStrategy": "exponential",
                    "baseDelay": "1s"
                }
            },
            "workflow": {
                "sync": {
                    "strategy": "comprehensive",
                    "autoCommit": True,
                    "generateCommitMessages": True,
                    "pushOnSync": True,
                    "backupConfigs": True
                },
                "planning": {
                    "autoNumbering": True,
                    "planPrefix": "FlowLoom",
                    "phaseStructure": "hierarchical"
                },
                "development": {
                    "verboseLogging": False,
                    "progressReporting": True,
                    "errorRecovery": "prompt"
                }
            },
            "compliance": {
                "enforcementLevel": "strict",
                "auditLogging": True,
                "contentFiltering": True,
                "credentialStorage": "secure",
                "ageVerification": True,
                "retentionPeriod": "30d"
            },
            "security": {
                "allowDangerousOperations": False,
                "requireExplicitPermissions": True,
                "networkIsolation": True,
                "sandboxedExecution": True
            },
            "environment": {
                "workDir": "${FLOWLOOM_WORK_DIR:-.meta-claude}",
                "configPaths": {
                    "commands": ".claude/commands",
                    "backup": "backup/.claude",
                    "plans": "plans"
                }
            }
        }
    
    def _deep_merge(self, base: Dict[str, Any], override: Dict[str, Any]) -> Dict[str, Any]:
        """Deep merge two configuration dictionaries."""
        result = base.copy()
        
        for key, value in override.items():
            if key in result and isinstance(result[key], dict) and isinstance(value, dict):
                result[key] = self._deep_merge(result[key], value)
            else:
                result[key] = value
        
        return result
    
    def _set_nested_value(self, config: Dict[str, Any], path: List[str], value: Any) -> None:
        """Set a nested configuration value using dot notation path."""
        current = config
        
        for key in path[:-1]:
            if key not in current:
                current[key] = {}
            current = current[key]
        
        current[path[-1]] = value
    
    def _to_camel_case(self, snake_str: str) -> str:
        """Convert snake_case to camelCase."""
        components = snake_str.split('_')
        return components[0] + ''.join(word.capitalize() for word in components[1:])
    
    def _parse_env_value(self, value: str) -> Union[str, int, float, bool]:
        """Parse environment variable value to appropriate type."""
        # Boolean values
        if value.lower() in ('true', 'yes', '1'):
            return True
        elif value.lower() in ('false', 'no', '0'):
            return False
        
        # Numeric values
        try:
            if '.' in value:
                return float(value)
            else:
                return int(value)
        except ValueError:
            pass
        
        # String value
        return value
    
    def get_config_info(self, result: LoadResult) -> Dict[str, Any]:
        """Get information about loaded configuration sources."""
        return {
            "sources": [
                {
                    "name": source.name,
                    "path": str(source.path) if source.path else None,
                    "loaded": source.loaded,
                    "error": source.error,
                    "keys": list(source.data.keys()) if source.loaded else []
                }
                for source in result.sources
            ],
            "validation": {
                "valid": result.validation.valid if result.validation else False,
                "errors": result.validation.errors if result.validation else [],
                "warnings": result.validation.warnings if result.validation else [],
                "deprecated_keys": result.validation.deprecated_keys if result.validation else []
            },
            "final_config": {
                "project_name": result.config.project.name if result.config else "Unknown",
                "agent_count": result.config.agents.maxConcurrent if result.config else 0,
                "coordination_strategy": result.config.agents.coordinationStrategy if result.config else "unknown"
            }
        }


class ConfigurationCache:
    """Caches loaded configuration to avoid repeated file I/O."""
    
    def __init__(self):
        self._cache: Dict[str, tuple[FlowLoomConfig, float]] = {}
        self._max_age = 300  # 5 minutes
    
    def get(self, cache_key: str) -> Optional[FlowLoomConfig]:
        """Get cached configuration if still valid."""
        if cache_key in self._cache:
            config, timestamp = self._cache[cache_key]
            import time
            if (time.time() - timestamp) < self._max_age:
                return config
            else:
                del self._cache[cache_key]
        return None
    
    def set(self, cache_key: str, config: FlowLoomConfig) -> None:
        """Cache configuration with timestamp."""
        import time
        self._cache[cache_key] = (config, time.time())
    
    def invalidate(self, cache_key: Optional[str] = None) -> None:
        """Invalidate cache entry or entire cache."""
        if cache_key:
            self._cache.pop(cache_key, None)
        else:
            self._cache.clear()


# Global configuration loader instance
_global_loader: Optional[ConfigurationLoader] = None
_global_cache = ConfigurationCache()


def get_loader(project_root: Optional[Path] = None) -> ConfigurationLoader:
    """Get global configuration loader instance."""
    global _global_loader
    if _global_loader is None or (project_root and _global_loader.project_root != project_root):
        _global_loader = ConfigurationLoader(project_root)
    return _global_loader


def load_config(
    project_root: Optional[Path] = None,
    config_file: Optional[Path] = None,
    use_cache: bool = True
) -> LoadResult:
    """Load FlowLoom configuration with global loader."""
    loader = get_loader(project_root)
    
    # Generate cache key
    cache_key = f"{project_root}:{config_file}"
    
    if use_cache:
        cached_config = _global_cache.get(cache_key)
        if cached_config:
            return LoadResult(config=cached_config)
    
    # Load fresh configuration
    result = loader.load_configuration(config_file)
    
    if result.config and use_cache:
        _global_cache.set(cache_key, result.config)
    
    return result


# Export main classes and functions
__all__ = [
    'ConfigurationLoader',
    'ConfigurationSource',
    'LoadResult',
    'EnvironmentVariableResolver',
    'ConfigurationCache',
    'get_loader',
    'load_config'
]