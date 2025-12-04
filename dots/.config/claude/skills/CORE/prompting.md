# Prompting Guidelines

## Core Principles

### 1. Be Specific and Clear
**Bad**: "Fix the code"
**Good**: "Fix the null pointer dereference in the handleRequest function in server.go:145"

### 2. Provide Context
**Bad**: "Add error handling"
**Good**: "Add error handling to the database connection in db.go, following the existing pattern used in cache.go"

### 3. State Desired Outcomes
**Bad**: "Make it better"
**Good**: "Reduce memory usage by implementing connection pooling with a maximum of 10 connections"

## Effective Prompting Patterns

### For Research and Exploration
```
"Find all files that handle user authentication"
"Explain how the DNS configuration works in globals.nix"
"Show me examples of how we use agenix for secrets"
```

### For Code Changes
```
"Add logging to the handleError function, using the same format as in server.go"
"Refactor the parseConfig function to return an error instead of panicking"
"Update the NixOS configuration to enable Docker on demeter"
```

### For Complex Tasks
```
"Implement a new feature that allows users to export data as JSON:
1. Add an export endpoint to the API
2. Create a JSON serialization function
3. Add tests for the new functionality
4. Update the documentation"
```

### For Debugging
```
"The build is failing with error 'undefined: foo'.
The error appears in cmd/main.go:42.
I recently added a new import but the function might not be exported."
```

## Progressive Refinement

Start broad, then narrow:
1. "How does authentication work in this codebase?"
2. "Show me the JWT token validation code"
3. "Add rate limiting to the token refresh endpoint"

## Asking for Explanations

### Request Specific Detail Levels
- "Give me a high-level overview of..."
- "Explain in detail how..."
- "What's the simplest explanation for..."

### Ask for Examples
- "Show me an example of..."
- "What would this look like if..."
- "Can you demonstrate..."

## Leveraging Skills

Invoke specific skills for domain expertise:
```
"Using NixOS best practices, add a new service to demeter"
"Following Go conventions, refactor this error handling"
"Create a note about today's deployment in denote format"
```

## Anti-Patterns to Avoid

### ❌ Vague Requests
"Make it work" - What specifically isn't working?

### ❌ Assuming Context
"Update the config" - Which config? What update?

### ❌ Multiple Unrelated Tasks
"Fix the bug, add a feature, and update docs" - Break into separate requests

### ❌ Unclear Success Criteria
"Improve performance" - By how much? What metrics?

## Iterative Development

Good prompting is often iterative:
1. Initial request: "Add logging to the API"
2. Refinement: "Use structured logging with JSON format"
3. Further refinement: "Include request ID and timestamp in each log entry"
4. Final touch: "Add log rotation with 7-day retention"

## Using Tools Effectively

### When to mention tools
- "Use grep to find all TODO comments"
- "Create a task list for this multi-step operation"
- "Read the globals.nix file to check the demeter configuration"

### Let the AI choose tools
Often better to state the goal and let the AI choose appropriate tools:
- "Find where we define DNS zones" (AI will use grep/glob appropriately)
- "Show me the athena NixOS configuration" (AI will read the right files)

## Questions and Clarification

Encourage questions:
- "If anything is unclear, please ask before proceeding"
- "Let me know if you need more context"
- "Ask questions if you're uncertain about the approach"

## Feedback Loop

Provide feedback to improve future interactions:
- "That worked perfectly, thanks!"
- "The approach was good, but next time please test before deploying"
- "This is too verbose, just show me the relevant code"

## Remember

The best prompts are:
1. **Specific** - Clear about what you want
2. **Contextual** - Provide relevant background
3. **Goal-oriented** - State the desired outcome
4. **Iterative** - Refine as you go
5. **Collaborative** - Work with the AI, not against it
