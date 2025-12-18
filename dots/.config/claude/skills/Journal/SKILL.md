---
name: Journal
description: Journelly-format journal entries. USE WHEN user wants to create journal entries, write reflections, or work with Journelly.org file.
---

# Journal Writing Workflow

## Purpose
Assist with creating and managing journal entries in Journelly format using org-mode.

### Context Detection

**This skill activates when:**
- User asks to create a journal entry, write a journal, or add to journal
- User mentions Journelly, journaling, or daily reflections
- User references `Journelly.org` file
- User asks about logging thoughts, daily notes, or personal reflections

## Journal Location
**Journal file**: `~/desktop/org/Journelly.org`

## Journelly Format

Journelly is an iOS app that stores journal entries in org-mode format as a single file with entries in reverse chronological order (newest first).

### Entry Structure

**Basic entry format**:
```org
* [YYYY-MM-DD Day HH:MM] @ Location
:PROPERTIES:
:LATITUDE: 48.86721377062119
:LONGITUDE: 2.1850910842231994
:WEATHER_TEMPERATURE: 5,8°C
:WEATHER_CONDITION: Cloudy
:WEATHER_SYMBOL: cloud
:END:
Entry content goes here...

Can have multiple paragraphs.

- Lists work
- [ ] Checkboxes work
- [X] Completed items

Images can be included:
[[file:Journelly.org.assets/images/IMAGE.jpeg]]
```

**Entry without properties**:
```org
* [YYYY-MM-DD Day HH:MM] @ Location

Simple entry without weather/GPS metadata.
```

### Key Components

**Heading**:
- Format: `* [YYYY-MM-DD Day HH:MM] @ Location`
- Timestamp: Full date and time in org-mode format
- Location: Can be a place name, address, or general location
- Examples:
  - `* [2025-12-08 Mon 15:30] @ Home`
  - `* [2025-12-08 Mon 09:15] @ Kyushu`
  - `* [2025-12-08 Mon 22:00] @ Rue Jean Bourguignon`

**Properties drawer** (optional):
- `:LATITUDE:` - GPS latitude
- `:LONGITUDE:` - GPS longitude
- `:WEATHER_TEMPERATURE:` - Temperature with unit (e.g., `5,8°C`)
- `:WEATHER_CONDITION:` - Weather description (e.g., `Cloudy`, `Clear`, `Partly Cloudy`)
- `:WEATHER_SYMBOL:` - Icon symbol (e.g., `cloud`, `sun.max`, `cloud.moon`)

**Content**:
- Free-form text in org-mode format
- Support for org-mode features: lists, checkboxes, links, code blocks
- Images stored in `Journelly.org.assets/images/` directory
- Can include hashtags (e.g., `#lang_en`)
- Can reference other org files with org-mode links

**IMPORTANT - Content Formatting Restrictions**:
- **NO sub-headings**: Journelly does NOT support `**` level 2 headings or any sub-headings within entries
- Use **indented lists** instead of sub-headings for structure:
  ```org
  - Section Title
    - Item 1
    - Item 2
  ```
- For bold emphasis in list items, use text formatting: `- *Bold Section Title*`
- Each journal entry must be a single `*` level 1 heading - no nested headings allowed

### Entry Order

Entries are in **reverse chronological order** - newest entries at the top of the file, after the header.

## File Header

The Journelly.org file starts with:
```org
#+TITLE: My Journal (via https://journelly.com)
#+STARTUP: showall
:journelly:
:doc_version: 1.0
:end:
```

This header should never be modified.

## Creating Entries

### Automatic Location and Weather Detection

Helper scripts are available to automatically get current location and weather:

**Get location (IP-based geolocation)**:
```bash
# Get city and coordinates
~/.config/claude/skills/Journal/tools/get-location
# Output: Saint-Denis (48.9356,2.3539)

# Get just coordinates
~/.config/claude/skills/Journal/tools/get-location --coords
# Output: 48.9356,2.3539

# Get as JSON
~/.config/claude/skills/Journal/tools/get-location --json
# Output: {"city":"Saint-Denis","lat":"48.9356","lon":"2.3539"}
```

**Get weather (from wttr.in)**:
```bash
# Get current weather
~/.config/claude/skills/Journal/tools/get-weather
# Output: 15°C Partly cloudy (cloud.sun)

# Get just temperature
~/.config/claude/skills/Journal/tools/get-weather --temperature
# Output: 15°C

# Get as JSON
~/.config/claude/skills/Journal/tools/get-weather --json
# Output: {"temperature":"15°C","condition":"Partly cloudy","symbol":"cloud.sun"}

# Get weather for specific location
~/.config/claude/skills/Journal/tools/get-weather Paris
```

**Notes**:
- Location uses IP-based geolocation (city-level accuracy, no GPS hardware required)
- Weather uses wttr.in service (no API key required)
- Both are cached (location: 1 hour, weather: 30 minutes) to reduce API calls
- Symbols are mapped to iOS SF Symbols for consistency with Journelly app

### Using journelly-manager (Recommended)

The `journelly-manager` tool provides batch mode operations for creating journal entries:

```bash
# Create simple entry with just location
~/.config/claude/skills/Journal/tools/journelly-manager create \
  ~/desktop/org/Journelly.org "Kyushu" "Entry content here"

# Create entry with weather/GPS metadata
~/.config/claude/skills/Journal/tools/journelly-manager create \
  ~/desktop/org/Journelly.org "Rue Jean Bourguignon" "Entry content" \
  --latitude=48.86721 \
  --longitude=2.18509 \
  --temperature="15,2°C" \
  --condition="Partly Cloudy" \
  --symbol="cloud.sun"

# Create entry with content from file
echo "My thoughts today..." > /tmp/journal.txt
~/.config/claude/skills/Journal/tools/journelly-manager create \
  ~/desktop/org/Journelly.org "Home" --content-file=/tmp/journal.txt

# Append to today's entry (if one exists from today)
~/.config/claude/skills/Journal/tools/journelly-manager append \
  ~/desktop/org/Journelly.org "Additional thoughts for today"

# Create entry with automatic location and weather
LOC_DATA=$(~/.config/claude/skills/Journal/tools/get-location --json)
WEATHER_DATA=$(~/.config/claude/skills/Journal/tools/get-weather --json)

~/.config/claude/skills/Journal/tools/journelly-manager create \
  ~/desktop/org/Journelly.org \
  "$(echo "$LOC_DATA" | jq -r .city)" \
  "Today's reflection with automatic metadata" \
  --latitude="$(echo "$LOC_DATA" | jq -r .lat)" \
  --longitude="$(echo "$LOC_DATA" | jq -r .lon)" \
  --temperature="$(echo "$WEATHER_DATA" | jq -r .temperature)" \
  --condition="$(echo "$WEATHER_DATA" | jq -r .condition)" \
  --symbol="$(echo "$WEATHER_DATA" | jq -r .symbol)"
```

**Output**: Returns JSON with success status and entry details.

### Manual Creation

If creating entries manually:

1. Open `~/desktop/org/Journelly.org`
2. After the file header (after `:end:`), insert new entry at the top
3. Use timestamp format: `[YYYY-MM-DD Day HH:MM]`
4. Add location after `@`
5. Optionally add PROPERTIES drawer
6. Write content below

**Get current timestamp**:
```bash
# Org-mode format
date +"[%Y-%m-%d %a %H:%M]"
```

## Entry Examples

### Structured entry with indented lists (RECOMMENDED for complex entries):
```org
* [2025-12-10 Wed 17:05] @ Paris
:PROPERTIES:
:LATITUDE: 48.8534
:LONGITUDE: 2.3488
:END:
Productive day with significant progress across multiple areas.

- Work (Tekton/CI-CD)
  - [X] Fixed workflow call in pipeline (priority 1)
  - [X] Created issue for cherry-pick workflow
  - [X] Standardized retest workflow across repositories

- Infrastructure & Homelab
  - [X] Set up Navidrome music streaming server
  - [X] Implemented color-scheme switcher on kyushu
  - [ ] Setup imap-filter (in progress)

- Personal Productivity
  - Updated imapfilter to archive emails by year
  - Researched Everyday Systems habit formation
  - Created note documenting implementation ideas

10 completed tasks, feeling productive!
```

**Note**: Use indented lists (with 2 spaces) instead of sub-headings (`**`) - Journelly doesn't support nested headings!

### Simple reflection:
```org
* [2025-12-08 Mon 15:30] @ Home

Had a productive day working on Claude skills. The Journal skill
is coming together nicely. Need to test the batch functions next.
```

### Work notes with checklist:
```org
* [2025-12-08 Mon 09:00] @ Kyushu

Today's focus:
- [X] Review pull requests
- [X] Fix bug in pipeline
- [ ] Write documentation
- [ ] Team meeting at 14:00

Making good progress on the telemetry work.
```

### Evening reflection with location data:
```org
* [2025-12-08 Mon 22:30] @ Rue Jean Bourguignon
:PROPERTIES:
:LATITUDE: 48.86721377062119
:LONGITUDE: 2.1850910842231994
:WEATHER_TEMPERATURE: 8,5°C
:WEATHER_CONDITION: Clear
:WEATHER_SYMBOL: moon.stars
:END:
Quiet evening. Spent time with family and worked on some personal
projects. Feeling good about the progress this week.

Tomorrow's priorities:
- House hunting follow-up
- Finish keyboard configuration
- Review Rhea setup
```

### Vacation entry with photos:
```org
* [2025-08-16 Sat 04:40] @ Rue Jean Bourguignon
:PROPERTIES:
:LATITUDE: 48.868139960083184
:LONGITUDE: 2.184074493463655
:WEATHER_TEMPERATURE: 16,8°C
:WEATHER_CONDITION: Clear
:WEATHER_SYMBOL: moon.stars
:END:
Good day at Plaisir with Malek and Ilyan. Everyone was tired at
the end but for good reasons.

[[file:Journelly.org.assets/images/CDB4EC5D-153A-4C35-A7E8-17BA94F7FEBD.jpeg]]

[[file:Journelly.org.assets/images/9CB95E09-CD4E-4868-BFE0-F0B129A8356B.jpeg]]

We need to find a way to have less mosquitoes bites for Ayla because
it is a lot and it is painful to see her with all thoses..
```

## Best Practices

### When to Journal

- **Morning**: Day planning, intentions, priorities
- **During work**: Quick thoughts, decisions, blockers
- **Evening**: Reflections, gratitude, learnings
- **Anytime**: Significant moments, insights, emotions

### Writing Guidelines

- **Be authentic**: Write for yourself, not an audience
- **Be specific**: Include details, context, emotions
- **Be brief**: Don't overthink it, capture the moment
- **Be consistent**: Regular entries build a valuable record
- **Use org-mode features**: Lists, checkboxes, links enhance entries

### Location Guidelines

- Use recognizable place names for your context
- Can be specific (address) or general (room name, city)
- Examples:
  - Work: `Kyushu` (machine name)
  - Home: `Home`, `Rue Jean Bourguignon`
  - Travel: `Amsterdam`, `R27`, `Boulevard de Turin`

### Privacy Considerations

- Journal contains personal thoughts and location data
- Stored locally and synced via Syncthing/iCloud
- No properties needed if you prefer not to log GPS/weather
- You control what details to include

## Common Use Cases

### Daily Reflection
```bash
# Simple end-of-day reflection
~/.config/claude/skills/Journal/tools/journelly-manager create \
  ~/desktop/org/Journelly.org "Home" \
  "Productive day. Made progress on the Journal skill. \
   Looking forward to testing it tomorrow."
```

### Work Log
```bash
# Log work accomplishments
~/.config/claude/skills/Journal/tools/journelly-manager create \
  ~/desktop/org/Journelly.org "Kyushu" \
  "Completed PR review. Fixed authentication bug. \
   Team meeting went well - discussed Q1 roadmap."
```

### Quick Thought
```bash
# Capture an idea quickly
~/.config/claude/skills/Journal/tools/journelly-manager create \
  ~/desktop/org/Journelly.org "Home" \
  "Idea: Create a dashboard for monitoring homelab services. \
   Could use Grafana + Prometheus."
```

### Adding to Today's Entry
```bash
# Append to existing entry from today
~/.config/claude/skills/Journal/tools/journelly-manager append \
  ~/desktop/org/Journelly.org \
  "Update: The dashboard idea is working! Initial prototype running."
```

## Integration with Other Systems

### Journelly iOS App

- Primary mobile interface for journal entries
- Automatically adds GPS and weather data
- Syncs via iCloud to make file available on Mac
- Can include photos captured on iPhone
- Creates proper org-mode format automatically

### Syncthing

- Journal file synced across devices
- Available on desktop for reading/searching
- Can edit from Emacs on desktop
- Changes sync back to iOS app

### Emacs

- Read and search journal with org-mode commands
- Use `org-sparse-tree` to filter entries
- Export to other formats (HTML, PDF)
- Integration with org-agenda if desired

## Searching and Reviewing

### Find Recent Entries
```bash
# Show last 10 entries (most recent)
grep -n "^\* \[" ~/desktop/org/Journelly.org | head -10
```

### Search by Date
```bash
# Find entries from December 2025
grep "^\* \[2025-12-" ~/desktop/org/Journelly.org

# Find entries from specific day
grep "^\* \[2025-12-08" ~/desktop/org/Journelly.org
```

### Search by Location
```bash
# Find all entries at Kyushu
grep "@ Kyushu" ~/desktop/org/Journelly.org
```

### Search Content
```bash
# Find entries mentioning specific topic
grep -i "claude" ~/desktop/org/Journelly.org

# Context around matches
grep -C 3 -i "homelab" ~/desktop/org/Journelly.org
```

### Search with Ripgrep
```bash
# Case-insensitive search with context
rg -i "keyboard" ~/desktop/org/Journelly.org

# Show only entries about work
rg "@ Kyushu" ~/desktop/org/Journelly.org -A 10
```

## Journelly vs Notes

**Use Journelly for**:
- Daily reflections and thoughts
- Personal experiences and emotions
- Time-based chronicle of life
- Quick captures without much structure
- Location-tagged memories

**Use Notes (denote) for**:
- Technical documentation
- Learning notes and research
- Reference material
- Project planning
- Knowledge that needs retrieval by topic

**They complement each other**: Journal captures the journey, Notes capture the knowledge.

## Org-Mode Features in Entries

### Lists
```org
* [2025-12-08 Mon 10:00] @ Home

Today's agenda:
- Morning: Focus work
- Afternoon: Meetings
- Evening: Family time
```

### Checkboxes
```org
* [2025-12-08 Mon 08:00] @ Kyushu

Weekly goals:
- [X] Complete feature implementation
- [X] Review 5 PRs
- [ ] Write documentation
- [ ] Update roadmap
```

### Links
```org
* [2025-12-08 Mon 16:00] @ Home

Referenced my [[file:~/desktop/org/notes/20251205T140000--nixos-config__nixos.org][NixOS config notes]]
for today's work.

Useful article: [[https://example.com][Link Title]]
```

### Code Blocks
```org
* [2025-12-08 Mon 14:00] @ Kyushu

Found a useful command today:

#+begin_src bash
nix build .#package-name
#+end_src

This made the build process much faster.
```

### Tables
```org
* [2025-12-08 Mon 12:00] @ Home

Tracking house options:

| Address          | Price | Score |
|------------------+-------+-------|
| Rue Victor Hugo  | 720k  |   7/10|
| Allée Perruchet  | 750k  |   9/10|
```

## Tips

1. **Write regularly**: Even short entries build a valuable record
2. **Don't overthink**: Capture thoughts as they come
3. **Use location meaningfully**: Helps trigger memories later
4. **Include context**: Future you will appreciate the details
5. **Reference other files**: Link to notes, todos, or external resources
6. **Use hashtags sparingly**: `#lang_en`, `#work`, `#personal` can help
7. **Add photos when meaningful**: Visual memories are powerful
8. **Review periodically**: Monthly or yearly reviews reveal patterns

## Example Workflow

### Mobile (Journelly iOS)
1. Open Journelly app
2. Tap to create new entry
3. Write thoughts (voice dictation works)
4. App automatically adds timestamp, location, weather
5. Can attach photos from camera or library
6. Entry syncs to iCloud → available on Mac

### Desktop (Claude/Emacs)
1. Ask Claude to create journal entry
2. Claude uses `journelly-manager` to add entry
3. Entry inserted at top of file (newest first)
4. File syncs back via iCloud/Syncthing
5. Entry appears in iOS app

### Hybrid
- Quick captures on mobile (always with you)
- Longer reflections on desktop (better for typing)
- Search and review on desktop (powerful tools)
- Photos from mobile, text from either

## Privacy & Sync

- Journal is a plain text file you control
- Location data is optional (from iOS app)
- Synced via iCloud (encrypted in transit)
- Can also use Syncthing for local-only sync
- No third-party service has access to content
- Backup with your regular backup strategy

Remember: Your journal is for you. Write what helps you think, remember, and grow.

## Examples

**Example 1: Creating a daily journal entry**
```
User: "Create my journal entry for today"
→ Uses Journelly format with date heading
→ Prompts for reflections and highlights
→ Adds structured sections (work, personal, learnings)
→ Saves to ~/desktop/org/notes/Journelly.org
→ Result: Well-structured journal entry for the day
```

**Example 2: Reviewing past entries**
```
User: "What did I write about last week?"
→ Searches Journelly.org for date range
→ Extracts entries from the past 7 days
→ Summarizes key themes and activities
→ Highlights important decisions or learnings
→ Result: Quick recap of the week's journal
```

**Example 3: Adding a reflection**
```
User: "Add a reflection about the Tekton work we just did"
→ Appends to today's journal entry
→ Captures what was accomplished
→ Notes lessons learned and insights
→ Links to relevant history or notes
→ Result: Reflection recorded for future reference
```
