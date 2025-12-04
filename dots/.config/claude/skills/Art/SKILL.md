---
name: Art
description: Visual content generation and diagram creation. USE WHEN user needs diagrams, flowcharts, technical visualizations, or any visual content to explain concepts.
---

# Art

Visual content generation skill for creating diagrams, flowcharts, and technical visualizations to support documentation and communication.

## Purpose

This skill helps create visual content when text alone isn't sufficient.

### Context Detection

**This skill activates when:**
- User asks for a diagram, flowchart, or visualization
- User mentions creating visual representations of concepts
- User wants to illustrate architecture, processes, or relationships
- User asks for Mermaid diagrams, ASCII art, or graphical explanations
- User explicitly says "show me a diagram" or "draw a flowchart"

## Use Cases

It's particularly useful for:
- Technical diagrams and architecture visualizations
- Flowcharts and process diagrams
- Concept maps and taxonomies
- Timeline visualizations
- Comparison charts

## Visual Approach

When creating diagrams, consider:
- **Clarity**: Make the diagram easy to understand
- **Purpose**: What insight should the visual provide?
- **Format**: Which format best serves the content (Mermaid, ASCII, etc.)?
- **Context**: Will this be in documentation, a presentation, or for quick reference?

## Supported Formats

### Mermaid Diagrams
Best for:
- Flowcharts
- Sequence diagrams
- State diagrams
- Class diagrams
- Gantt charts

### ASCII Art
Best for:
- Simple diagrams in plain text
- Terminal-friendly visualizations
- Code comments

### Structured Text
Best for:
- Comparisons (tables)
- Hierarchies (indented lists)
- Timelines (ordered lists)

## Workflow Routing

| Workflow | Trigger | File |
|----------|---------|------|
| **Visualize** | "create a diagram", "visualize this" | `workflows/Visualize.md` |
| **Mermaid** | "mermaid diagram", "flowchart" | `workflows/Mermaid.md` |

## Examples

**Example 1: Architecture Diagram**
```
User: "Create a diagram showing the NixOS deployment flow"
→ Invokes Visualize workflow
→ Determines Mermaid flowchart is appropriate
→ Creates flowchart showing: local changes → build → deploy → activate
```

**Example 2: Comparison Chart**
```
User: "Compare stable vs unstable NixOS approaches"
→ Invokes Visualize workflow
→ Creates comparison table
→ Shows trade-offs clearly
```

**Example 3: Process Flow**
```
User: "Show the git commit workflow as a diagram"
→ Invokes Mermaid workflow
→ Creates flowchart with decision points
→ Shows happy path and error handling
```

## Integration

This skill complements:
- **Documentation**: Add diagrams to markdown docs
- **Architecture planning**: Visualize system design
- **Troubleshooting**: Map out problem flows
- **Communication**: Explain complex concepts visually
