# RGB LED Map Guide for Moonlander Keymap

## Overview

The keymap now uses a more maintainable array-based approach for defining RGB LED colors per key. This makes it much easier to visualize and modify the LED configuration.

## How to Define Colors

### 1. Using Predefined Color Constants

The following color constants are available:

```c
RGB_OFF           // 0, 0, 0 - No color
RGB_RED           // 255, 0, 0 - Red (defined in QMK)
RGB_ORANGE_YELLOW // 213, 144, 0
RGB_GREEN         // 27, 213, 0
RGB_GREEN_LIGHT   // 206, 255, 206
RGB_CYAN          // 0, 255, 167
RGB_BLUE_LIGHT    // 0, 113, 255
RGB_BLUE          // 0, 55, 255
RGB_BLUE_BRIGHT   // 0, 183, 255
RGB_WHITE         // 255, 255, 255
RGB_YELLOW        // 255, 199, 0
RGB_YELLOW_GREEN  // 183, 255, 0
RGB_PURPLE        // 13, 0, 255
RGB_GREEN_MEDIA   // 27, 213, 0
RGB_PURPLE_VOL    // 170, 0, 255
```

### 2. LED Map Structure

The LED map is a 3D array: `ledmap[layer][led_index][rgb]`

- **layer**: The keyboard layer (QWER, NUMB, SYMB, NAVI, etc.)
- **led_index**: The LED number (0-71)
- **rgb**: RGB color values [r, g, b]

### 3. LED Matrix Layout

```
   ┌────┬────┬────┬────┬────┬────┬────┐             ┌────┬────┬────┬────┬────┬────┬────┐
   │ 0  │ 5  │ 10 │ 15 │ 20 │ 25 │ 29 │             │ 65 │ 61 │ 56 │ 51 │ 46 │ 41 │ 36 │
   ├────┼────┼────┼────┼────┼────┼────┤             ├────┼────┼────┼────┼────┼────┼────┤
   │ 1  │ 6  │ 11 │ 16 │ 21 │ 26 │ 30 │             │ 66 │ 62 │ 57 │ 52 │ 47 │ 42 │ 37 │
   ├────┼────┼────┼────┼────┼────┼────┤             ├────┼────┼────┼────┼────┼────┼────┤
   │ 2  │ 7  │ 12 │ 17 │ 22 │ 27 │ 31 │             │ 67 │ 63 │ 58 │ 53 │ 48 │ 43 │ 38 │
   ├────┼────┼────┼────┼────┼────┼────┘             └────┼────┼────┼────┼────┼────┼────┤
   │ 3  │ 8  │ 13 │ 18 │ 23 │ 28 │                       │ 64 │ 59 │ 54 │ 49 │ 44 │ 39 │
   ├────┼────┼────┼────┼────┼────┼─────────┐   ┌─────────┼────┼────┼────┼────┼────┼────┤
   │ 4  │ 9  │ 14 │ 19 │ 24 │    │   35    │   │   71    │    │ 60 │ 55 │ 50 │ 45 │ 40 │
   └────┴────┴────┴────┴────┼────┼────┬────┤   ├────┬────┼────┼────┴────┴────┴────┴────┘
                            │ 32 │ 33 │ 34 │   │ 68 │ 69 │ 70 │
                            └────┴────┴────┘   └────┴────┴────┘
```

## Example: Adding Colors to a Layer

### Example 1: Simple Layer Configuration

```c
[MY_LAYER] = {
    // Set specific keys to colors
    [0] = {RGB_RED},      // Top-left key red
    [36] = {RGB_BLUE},    // Top-right key blue
    [7] = {RGB_GREEN},    // Home row left
    [43] = {RGB_GREEN},   // Home row right
},
```

### Example 2: Full Row Configuration

```c
[MY_LAYER] = {
    // Row 1: All yellow
    [1] = {RGB_YELLOW}, [6] = {RGB_YELLOW}, [11] = {RGB_YELLOW}, 
    [16] = {RGB_YELLOW}, [21] = {RGB_YELLOW}, [26] = {RGB_YELLOW},
    [62] = {RGB_YELLOW}, [57] = {RGB_YELLOW}, [52] = {RGB_YELLOW}, 
    [47] = {RGB_YELLOW}, [42] = {RGB_YELLOW}, [37] = {RGB_YELLOW},
},
```

### Example 3: Custom RGB Values

If you need a color not in the constants:

```c
[MY_LAYER] = {
    [0] = {128, 64, 200},  // Custom purple color
    [5] = {255, 128, 0},   // Custom orange color
},
```

## Adding a New Color Constant

To add a new color constant, add it near the top of keymap.c:

```c
#define RGB_MY_COLOR 123, 45, 67
```

## Tips

1. **Unspecified LEDs default to {0, 0, 0} (off)** - You only need to define the keys you want to light up
2. **Use comments** - Group related keys with comments for better readability
3. **Test incrementally** - Add a few keys at a time and test to ensure they work as expected
4. **Refer to the matrix diagram** - Keep the ASCII diagram handy when mapping LED numbers

## Benefits of This Approach

- ✅ **Visual clarity**: See all layer colors in one place
- ✅ **Easy modifications**: Change a color by editing one line
- ✅ **Named colors**: Use descriptive names instead of RGB values
- ✅ **Less repetition**: No more copy-pasting `rgb_matrix_set_color()` calls
- ✅ **Consistency**: Same approach as voyagevoyage.c
