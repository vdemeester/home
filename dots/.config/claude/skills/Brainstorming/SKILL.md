---
name: Brainstorming
description: Interactive design refinement through collaborative dialogue before implementation. USE WHEN user wants to design a feature OR plan architecture OR explore approaches OR needs help thinking through implementation before coding OR asks "how should I" build something. Asks questions one at a time, proposes alternatives, validates incrementally.
---

# Brainstorming

Turn ideas into fully formed designs through natural collaborative dialogue before writing code.

## Overview

Help refine vague ideas into concrete, implementable designs by:
- Understanding project context first
- Asking focused questions (one at a time)
- Proposing alternative approaches with trade-offs
- Validating design incrementally
- Documenting the final validated design

## The Process

### Phase 1: Understanding the Idea

**Check context first:**

```bash
# Review current project state
git log --oneline -10

# Check existing patterns
rg "similar-feature" --files-with-matches

# Examine recent changes
git diff main...HEAD --stat
```

**Ask questions one at a time:**
- Prefer multiple choice when possible
- Open-ended questions are fine for exploration
- Focus on: purpose, constraints, success criteria
- Don't overwhelm with multiple questions at once

**Key questions to explore:**
- What problem does this solve?
- Who are the users?
- What are the constraints (performance, compatibility, complexity)?
- What defines success?
- Are there existing patterns to follow?

### Phase 2: Exploring Approaches

**Propose 2-3 different approaches with trade-offs:**

1. Lead with your recommended option
2. Explain reasoning clearly
3. Present trade-offs honestly
4. Consider existing patterns in the codebase

**Template for presenting options:**

```
I see [number] approaches:

1. **[Approach Name]** (Recommended)
   + [Advantage 1]
   + [Advantage 2]
   - [Disadvantage 1]

2. **[Alternative Approach]**
   + [Advantage]
   - [Disadvantage]

3. **[Another Alternative]**
   + [Advantage]
   - [Disadvantage]

I recommend #1 because [specific reasoning based on context].
```

**Example:**
```
I see three approaches for implementing caching:

1. **Redis with TTL** (Recommended)
   + Persistent across restarts
   + Already using Redis for sessions
   + Supports complex invalidation patterns
   - Adds network latency

2. **In-memory cache (sync.Map)**
   + Fastest access (no network)
   + Simple implementation
   - Lost on restart
   - No sharing across instances

3. **HTTP cache headers + CDN**
   + Offloads to edge
   + Reduces backend load
   - Less control over invalidation
   - Requires public endpoints

I recommend #1 because you need cache persistence and already
have Redis infrastructure in place.
```

### Phase 3: Presenting the Design

**Break design into digestible sections (200-300 words each):**

1. **Architecture Overview**: High-level structure, components, relationships
2. **Component Breakdown**: Detailed responsibilities of each part
3. **Data Flow**: How information moves through the system
4. **Error Handling**: How failures are detected and handled
5. **Testing Strategy**: How to verify correctness

**Validate incrementally:**
- Present one section at a time
- After each section: "Does this look right so far?"
- Be ready to go back and clarify
- Don't proceed if user has concerns

**Example section presentation:**

```
## Architecture Overview

The caching layer sits between the API handlers and the database:

```
[API Handler] → [Cache Middleware] → [Database]
                       ↓
                   [Redis]
```

The middleware:
- Checks Redis for cached responses
- On cache miss, calls database and caches result
- Invalidates cache on write operations
- Uses key pattern: `cache:v1:{resource}:{id}`

Does this high-level structure look right so far?
```

### Phase 4: After Design Validation

**Document the validated design:**

```bash
# Create design document
TOPIC="feature-name"
DATE=$(date +%Y-%m-%d)
DOC_PATH="docs/plans/${DATE}-${TOPIC}-design.md"

cat > "$DOC_PATH" <<'EOF'
# [Feature Name] Design

**Date**: [Date]
**Status**: Validated

## Problem Statement
[What we're solving]

## Architecture
[Validated architecture]

## Components
[Component breakdown]

## Data Flow
[How data moves]

## Error Handling
[Failure scenarios]

## Testing Strategy
[How to verify]

## Implementation Notes
[Important considerations]
EOF

git add "$DOC_PATH"
```

**Offer next steps:**
- "Ready to set up for implementation?"
- "Should I create a detailed implementation plan?"
- "Want to break this into phases?"

## Key Principles

### YAGNI Ruthlessly
**You Aren't Gonna Need It**

During brainstorming, actively remove features that:
- Sound nice but aren't required for the core use case
- Could be added later if needed
- Add complexity without clear value
- Are "just in case" features

**Example:**
```
User: "And we should probably add support for multiple formats: JSON, XML, YAML, TOML..."
Assistant: "Let's start with just JSON since that's what you're using now. We can add other formats later if needed. Simpler is better."
```

### One Question at a Time

**Don't do this:**
```
What problem are you solving? Who are the users? What are the
performance requirements? Do you need offline support? What's
the expected scale?
```

**Do this instead:**
```
What's the primary problem this feature solves?

[Wait for answer]

Got it. Who will be using this feature - developers, end users, or both?

[Wait for answer]

Thanks. Are there any performance constraints I should know about?
```

### Multiple Choice When Possible

**Better:**
```
What's more important for this feature:

A) Speed (sub-100ms response time)
B) Reliability (never lose data)
C) Simplicity (easy to maintain)

Or something else?
```

**Instead of:**
```
What are your requirements for this feature?
```

### Explore Alternatives Always

Never settle on first approach. Always:
1. Identify at least 2-3 alternatives
2. Present trade-offs
3. Recommend one with reasoning
4. Let user decide

### Be Flexible

If user seems confused or uncomfortable:
- Back up and clarify
- Ask simpler questions
- Provide examples
- Don't force forward progress

## Integration with Other Skills

**Before brainstorming:**
- Check project context with **Grep** and **Read** tools
- Review architecture patterns in codebase

**After brainstorming:**
- Use **TestDrivenDevelopment** for implementation
- Create implementation plan (can be added as workflow)
- Consider using **EnterPlanMode** for complex features

**During brainstorming:**
- Use **Notes** skill to capture key decisions
- Reference **CORE** principles for technology choices

## Examples

**Example 1: Feature design request**
```
User: "I want to add caching to the API"

→ Invoke Brainstorming skill
→ Check existing codebase for caching patterns
→ Ask: "What's driving this: reducing database load, improving response time, or both?"
→ User: "Response time"
→ Ask: "Should cache invalidate on writes, use TTL, or both?"
→ User: "TTL with manual invalidation for writes"
→ Propose three approaches: Redis, in-memory, CDN
→ Present recommended approach (Redis) with reasoning
→ Break down architecture into sections:
  1. Middleware structure
  2. Cache key strategy
  3. Invalidation logic
  4. Error handling
→ Validate each section with user
→ Document final design in docs/plans/
→ Offer to create implementation plan
```

**Example 2: Architecture planning**
```
User: "How should I structure the authentication service?"

→ Invoke Brainstorming skill
→ Check existing patterns in codebase
→ Ask: "What type of auth: OAuth, JWT, session-based, or something else?"
→ User: "JWT"
→ Ask: "Will you need refresh tokens or just access tokens?"
→ User: "Both"
→ Propose three architectural approaches:
  1. Stateless JWT with refresh token store
  2. Fully stateless (no refresh token persistence)
  3. Hybrid (JWT + session validation)
→ Recommend #1 with reasoning
→ Present design in sections:
  1. Token generation flow
  2. Validation middleware
  3. Refresh token rotation
  4. Secret management (with agenix reference)
→ Validate each section
→ Document design
→ Create implementation checklist
```

**Example 3: Exploring approaches before committing**
```
User: "I need to add real-time notifications"

→ Invoke Brainstorming skill
→ Ask: "How many concurrent users do you expect?"
→ User: "Maybe 100-200"
→ Ask: "Do clients need bi-directional communication or just server-to-client?"
→ User: "Just server to client for now"
→ Propose approaches:
  1. WebSockets (most common)
  2. Server-Sent Events (simpler for one-way)
  3. Long polling (widest compatibility)
→ Recommend SSE for your use case (simpler, one-way)
→ Present design:
  1. SSE endpoint structure
  2. Connection management
  3. Message format
  4. Reconnection handling
→ User validates
→ Document and proceed to implementation
```

**Example 4: Refactoring planning**
```
User: "This module has gotten too complex, help me think through how to refactor it"

→ Invoke Brainstorming skill
→ Read the current module code
→ Ask: "What's the main pain point: testing, understanding, or modifying?"
→ User: "Hard to test, too many dependencies"
→ Propose refactoring strategies:
  1. Extract interfaces for dependencies (DI approach)
  2. Split into smaller modules by domain
  3. Event-driven with message passing
→ Recommend #2 (splitting) based on code structure
→ Present refactoring plan:
  1. Identify domain boundaries
  2. Define module interfaces
  3. Migration strategy (incremental)
  4. Testing approach for each phase
→ Validate plan
→ Create phased implementation checklist
```

## Common Pitfalls to Avoid

**Don't:**
- Jump to implementation before understanding
- Present only one approach
- Overwhelm with too many options (>3)
- Skip validation of each design section
- Forget to document the final design
- Add features without clear requirements (fight YAGNI violations)

**Do:**
- Take time to understand context
- Present clear trade-offs
- Recommend an approach with reasoning
- Validate incrementally
- Document thoroughly
- Remove unnecessary complexity
