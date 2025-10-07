# Quick Start: Modifying LED Colors

This guide shows how to quickly modify LED colors in the refactored keymap.c file.

## Example 1: Change a Single Key Color

**Task:** Change the top-left key (LED 0) from OFF to RED in the QWER layer.

**Before refactoring:**
You would need to find the QWER case in `rgb_matrix_indicators_user()` and add:
```c
rgb_matrix_set_color(0, 255, 0, 0);
```

**After refactoring:**
Just edit the ledmap array:
```c
[QWER] = {
    [0] = {RGB_RED},  // Changed from RGB_OFF
    ...
}
```

## Example 2: Change All Number Keys to a Different Color

**Task:** Change all number keys (47-49, 52-54, 57-59, 60) from YELLOW to GREEN in NUMB layer.

**Before refactoring:**
Find and replace 10 individual lines:
```c
rgb_matrix_set_color(47, 255, 199, 0);  → rgb_matrix_set_color(47, 0, 255, 0);
rgb_matrix_set_color(48, 255, 199, 0);  → rgb_matrix_set_color(48, 0, 255, 0);
// ... 8 more lines to change
```

**After refactoring:**
Change all in one visual block:
```c
[NUMB] = {
    // Numbers - green (changed from yellow)
    [47] = {RGB_GREEN}, [48] = {RGB_GREEN}, [49] = {RGB_GREEN},
    [52] = {RGB_GREEN}, [53] = {RGB_GREEN}, [54] = {RGB_GREEN},
    [57] = {RGB_GREEN}, [58] = {RGB_GREEN}, [59] = {RGB_GREEN},
    [60] = {RGB_WHITE}, // 0
    ...
}
```

## Example 3: Add a New Color Constant

**Task:** Add a custom pink color.

1. Add the constant at the top:
```c
#define RGB_PINK 255, 105, 180
```

2. Use it in the ledmap:
```c
[SYMB] = {
    [11] = {RGB_PINK},
    ...
}
```

## Example 4: Add LED Configuration for MOUS Layer

**Task:** Add blue highlighting for mouse buttons in the MOUS layer.

Currently, MOUS has no LED configuration. Add it:

```c
// NAVI layer
[NAVI] = {
    ...existing config...
},
// MOUS layer
[MOUS] = {
    // Mouse buttons - blue
    [33] = {RGB_BLUE_LIGHT},  // MS_BTN1
    [34] = {RGB_BLUE_BRIGHT}, // MS_BTN3
    [68] = {RGB_BLUE_BRIGHT}, // MS_BTN3
    [69] = {RGB_BLUE_LIGHT},  // MS_BTN2
    // Mouse movement - cyan
    [52] = {RGB_CYAN}, // MS_UP
    [57] = {RGB_CYAN}, // MS_LEFT
    [58] = {RGB_CYAN}, // MS_DOWN
    [53] = {RGB_CYAN}, // MS_RGHT
},
```

Then update the switch statement to include MOUS:
```c
case QWER:
case NUMB:
case SYMB:
case NAVI:
case MOUS:  // Add this line
    rgb_matrix_set_color_all(RGB_OFF);
    set_layer_color(layer);
    break;
```

## Example 5: Create a Visual Pattern

**Task:** Make the home row keys gradient from blue to green in QWER layer.

```c
[QWER] = {
    ...
    // Row 2: home row gradient
    [7] = {0, 100, 255},    // More blue
    [12] = {0, 150, 200},   // Blue-green
    [17] = {0, 200, 150},   // Green-blue
    [22] = {0, 255, 100},   // More green
    [58] = {0, 255, 100},   // More green
    [53] = {0, 200, 150},   // Green-blue
    [48] = {0, 150, 200},   // Blue-green
    [43] = {0, 100, 255},   // More blue
    ...
}
```

## Tips

1. **Find LED numbers:** Refer to the ASCII diagram in keymap.c (lines 225-237)
2. **Group visually:** Organize keys by row or function with comments
3. **Test incrementally:** Change a few keys at a time and build/flash
4. **Use named colors:** Prefer RGB_BLUE over `0, 55, 255` for clarity
5. **Default is OFF:** Only define keys you want to light up

## LED Matrix Reference (Quick Lookup)

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

## Available Colors

```c
RGB_OFF, RGB_RED, RGB_ORANGE_YELLOW, RGB_GREEN, RGB_GREEN_LIGHT,
RGB_CYAN, RGB_BLUE_LIGHT, RGB_BLUE, RGB_BLUE_BRIGHT, RGB_WHITE,
RGB_YELLOW, RGB_YELLOW_GREEN, RGB_PURPLE, RGB_GREEN_MEDIA, RGB_PURPLE_VOL
```
