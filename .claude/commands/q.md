# FlowLoom Quick Help & Command Search

You are helping with FlowLoom command discovery. 

**Arguments:** `$ARGUMENTS`

## Task

If no arguments provided, show the general help below.
If arguments provided, perform fuzzy search of FlowLoom commands:

1. Use `mcp__filesystem__directory_tree .claude/commands` to get command structure
2. Use `Grep` to search for the term in command file names and content  
3. For single match: read the file and show detailed usage
4. For multiple matches: list them with brief descriptions
5. For no matches: suggest similar commands

## General Help (No Arguments)

# 🚀 FlowLoom Quick Reference

## 🎯 Essential Commands (Memorize First)

**Must-know commands:**
- `/stat` → System status (most important) 
- `/cont` → Load startup context
- `/com` → Full command reference
- `/q <term>` → Search commands (this!)

**Quick status checks:**
- `/as` → App | `/cs` → Coordination | `/ds` → Docker
- `/ps` → Plan | `/ss` → Session | `/ws` → Worker

## 📋 Category Patterns

**Core workflows:**
- `/c:*` → Coordination (`init`, `dispatch`, `status`)
- `/m:*` → Memory (`query`, `monitor`, `track`)  
- `/p:*` → Planning (`arch`, `highlevel`, `status`)
- `/s:*` → Session (`start`, `stop`, `status`)
- `/w:*` → Worker (`register`, `status`, `complete`)

**Development:**
- `/mo:*` → Mode (`pair`, `opus`, `story`)
- `/d:*` → Docker (`setup`, `status`, `logs`)
- `/dv:*` → Dev (`setup`, `add`, `update`)
- `/g:*` → Git (`sync`, `backup`, `cleanup`)

## 🔍 How to Search

```bash
/q status        # All status commands
/q docker        # Docker-related commands  
/q memory        # Memory management
/q plan          # Planning commands
/q coord         # Coordination workflow
```

## 🔄 Essential Workflows

**Start working:**
```
/cont → /s:start → /mo:pair
```

**Check everything:**
```  
/stat → /as → /cs → /ds
```

**Plan architecture:**
```
/p:arch → /p:highlevel → /p:status
```

**Coordinate team:**
```
/c:init → /c:dispatch task → /c:status
```

## 💡 Pro Tips

1. **Search first** - Use `/q <term>` to discover commands
2. **Learn patterns** - Commands follow consistent naming
3. **Start with status** - `/stat` for system health
4. **Recover context** - `/cont` after interruptions  
5. **Chain workflows** - Commands work together

---

**🔍 Search any term with `/q <term>`**  
**📋 Full reference with `/com`**  
**🎯 System status with `/stat`**