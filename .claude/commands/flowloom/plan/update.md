# Update Memory and Plans

This is a reminder to update memory and plan documents with the latest progress. As part of your ongoing work, you should:

1. **Update Memory Tool**: 
   - Add new entities for any concepts, components, or systems you've learned about
   - Create relations between related entities
   - Add observations to existing entities about your recent discoveries

2. **Update Plan Documents**: 
   - Update status markers for completed tasks with ✅ and [x] checkboxes
   - Add implementation notes to completed tasks with specific details
   - Detail any challenges encountered and how they were overcome
   - Note any unexpected insights or learnings
   - Update the "Current Implementation Status" section
   - Follow the update process in `${FLOWLOOM_WORK_DIR:-.meta-claude}/docs/plan_file_naming_convention.md`
   - **Mark completion status using standard completion indicators**

3. **Document Current Progress**:
   - What has been completed since the last update?
   - What challenges were encountered and how were they addressed?
   - What expected benefits have been realized from recent work?

4. **Document Next Steps**:
   - What are the immediate next tasks to be completed?
   - What dependencies exist for these tasks?
   - Are there any risks or potential issues to be aware of?

## Plan Completion Status Markup

When updating plans, use these standardized completion markers that the docs:change command recognizes:

### **Completion Status Indicators**
- **✅ COMPLETE** - For fully completed phases/sections
- **✅ IMPLEMENTATION COMPLETE** - For completed implementation work  
- **✅ PHASE COMPLETE** - For completed phases
- **🔄 IN PROGRESS** - For actively worked sections
- **📝 PLANNED** - For future planned work
- **⚠️ BLOCKED** - For blocked or dependent work

### **Section Status Examples**
```markdown
## Phase 1: Database Schema ✅ COMPLETE
- [x] Create GoogleCalendarDataProvider model ✅
- [x] Add google_account_id field ✅
- [x] Write database migration ✅

## Phase 2: OAuth Implementation 🔄 IN PROGRESS  
- [x] Implement credential extraction ✅
- [ ] Add multi-account support 🔄
- [ ] Complete error handling 📝

## Phase 3: Testing Framework ✅ IMPLEMENTATION COMPLETE
All testing infrastructure and test cases have been successfully implemented.
```

### **Plan Header Status**
Add status indicators to plan headers:
```markdown
# AUP-1250: Google Calendar OAuth Integration ✅ IMPLEMENTATION COMPLETE

## Current Implementation Status: ✅ COMPLETE
All phases of implementation have been successfully completed as of [date].
```

### **Task Status Markers**
- `[x]` - Completed tasks (with optional ✅ for emphasis)
- `[ ]` - Pending tasks  
- `[~]` - In progress tasks
- `[!]` - Blocked tasks

Remember to be thorough and precise in your updates. Good documentation with proper status markers enhances future work and ensures the docs:change command can accurately identify completion states for comprehensive documentation generation.