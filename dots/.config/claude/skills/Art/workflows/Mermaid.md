# Mermaid Workflow

Create Mermaid diagrams for technical documentation and visualization.

## When to Use Mermaid

Mermaid is ideal for:
- **Flowcharts**: Process flows, algorithms, decision trees
- **Sequence diagrams**: API interactions, protocol flows
- **State diagrams**: System states and transitions
- **Class diagrams**: Object relationships
- **Gantt charts**: Project timelines
- **Git graphs**: Branch and merge visualizations

## Mermaid Diagram Types

### Flowchart (Most Common)

```mermaid
flowchart LR
    A[Start] --> B[Process]
    B --> C{Decision}
    C -->|Yes| D[Action 1]
    C -->|No| E[Action 2]
```

**Directions:**
- `LR`: Left to Right
- `TD` or `TB`: Top Down/Top to Bottom
- `RL`: Right to Left
- `BT`: Bottom to Top

**Node Shapes:**
- `[Text]`: Rectangle
- `(Text)`: Rounded rectangle
- `{Text}`: Diamond (decision)
- `([Text])`: Stadium shape
- `[[Text]]`: Subroutine
- `[(Text)]`: Cylindrical (database)

### Sequence Diagram

```mermaid
sequenceDiagram
    participant Client
    participant API
    participant DB

    Client->>API: Request
    API->>DB: Query
    DB-->>API: Result
    API-->>Client: Response
```

**Arrow types:**
- `->`: Solid line
- `-->`: Dotted line
- `->>`: Solid arrow
- `-->>`: Dotted arrow

### State Diagram

```mermaid
stateDiagram-v2
    [*] --> Idle
    Idle --> Running: start()
    Running --> Paused: pause()
    Paused --> Running: resume()
    Running --> [*]: stop()
```

### Class Diagram

```mermaid
classDiagram
    class Animal {
        +String name
        +int age
        +makeSound()
    }
    class Dog {
        +String breed
        +bark()
    }
    Animal <|-- Dog
```

### Gantt Chart

```mermaid
gantt
    title Project Timeline
    dateFormat YYYY-MM-DD
    section Phase 1
    Task 1: 2024-01-01, 30d
    Task 2: 2024-01-15, 20d
    section Phase 2
    Task 3: 2024-02-01, 25d
```

## Best Practices

1. **Keep it simple**: Don't overcomplicate
2. **Use clear labels**: Make text descriptive
3. **Logical flow**: Follow natural reading direction
4. **Group related items**: Use subgraphs when appropriate
5. **Test rendering**: Ensure it renders correctly

## Common Patterns

### Decision Tree

```mermaid
flowchart TD
    Start[User Request] --> Auth{Authenticated?}
    Auth -->|No| Login[Redirect to Login]
    Auth -->|Yes| Check{Has Permission?}
    Check -->|No| Error[Show Error]
    Check -->|Yes| Process[Process Request]
```

### API Flow

```mermaid
sequenceDiagram
    participant C as Client
    participant A as API
    participant D as Database

    C->>A: POST /api/data
    A->>A: Validate Input
    A->>D: INSERT data
    D-->>A: Success
    A-->>C: 201 Created
```

### State Machine

```mermaid
stateDiagram-v2
    [*] --> Draft
    Draft --> Review: Submit
    Review --> Approved: Approve
    Review --> Draft: Reject
    Approved --> Published: Publish
    Published --> [*]
```

## Output Format

Present Mermaid diagrams with:
1. Brief description of what it shows
2. The Mermaid code block
3. Explanation of key elements

```
Here's a flowchart showing the deployment process:

[Mermaid diagram code]

The diagram illustrates:
- [Key element 1 explanation]
- [Key element 2 explanation]
```
