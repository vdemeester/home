# RGB LED Map Guide for Moonlander Keymap

## Overview

The keymap uses a visual layout-based approach for defining RGB LED colors per key, using the `LED_LAYOUT` macro that matches the keyboard's physical layout. This makes it extremely easy to visualize and modify the LED configuration.

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

There's also a shorthand: `___` which expands to `{RGB_OFF}` for cleaner code.

### 2. LED_LAYOUT Macro

The LED map uses the `LED_LAYOUT` macro that matches the visual representation of the keyboard, just like the `LAYOUT` macro for keymaps. This provides an intuitive, visual way to see which keys have which colors.

**Structure:**
```c
[LAYER] = LED_LAYOUT(
    // Row 0 - top row
    k00, k05, k10, k15, k20, k25, k29,    k65, k61, k56, k51, k46, k41, k36,
    // Row 1
    k01, k06, k11, k16, k21, k26, k30,    k66, k62, k57, k52, k47, k42, k37,
    // Row 2 - home row
    k02, k07, k12, k17, k22, k27, k31,    k67, k63, k58, k53, k48, k43, k38,
    // Row 3
    k03, k08, k13, k18, k23, k28,              k64, k59, k54, k49, k44, k39,
    // Row 4 - bottom row
    k04, k09, k14, k19, k24,      k35,    k71,      k60, k55, k50, k45, k40,
    // Thumb cluster
                        k32, k33, k34,    k68, k69, k70
)

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

Using the visual LED_LAYOUT macro, you can easily see which keys have which colors:

```c
[MY_LAYER] = LED_LAYOUT(
    {RGB_RED},  ___,  ___,  ___,  ___,  ___,  ___,    ___,  ___,  ___,  ___,  ___,  ___,  {RGB_BLUE},
    ___,        ___,  ___,  ___,  ___,  ___,  ___,    ___,  ___,  ___,  ___,  ___,  ___,  ___,
    ___,        {RGB_GREEN}, ___,  ___,  ___,  ___,  ___,    ___,  ___,  ___,  ___,  ___,  {RGB_GREEN}, ___,
    ___,        ___,  ___,  ___,  ___,  ___,              ___,  ___,  ___,  ___,  ___,  ___,
    ___,        ___,  ___,  ___,  ___,        ___,    ___,        ___,  ___,  ___,  ___,  ___,
                                  ___,  ___,  ___,    ___,  ___,  ___
),
```

### Example 2: Full Row Configuration

```c
[MY_LAYER] = LED_LAYOUT(
    ___,            ___,            ___,            ___,            ___,            ___,            ___,                           ___,            ___,            ___,            ___,            ___,            ___,            ___,
    {RGB_YELLOW},   {RGB_YELLOW},   {RGB_YELLOW},   {RGB_YELLOW},   {RGB_YELLOW},   {RGB_YELLOW},   ___,                           ___,            {RGB_YELLOW},   {RGB_YELLOW},   {RGB_YELLOW},   {RGB_YELLOW},   {RGB_YELLOW},   {RGB_YELLOW},
    ___,            ___,            ___,            ___,            ___,            ___,            ___,                           ___,            ___,            ___,            ___,            ___,            ___,            ___,
    ___,            ___,            ___,            ___,            ___,            ___,                                                           ___,            ___,            ___,            ___,            ___,            ___,
    ___,            ___,            ___,            ___,            ___,                            ___,                           ___,                            ___,            ___,            ___,            ___,            ___,
                                                                    ___,            ___,            ___,                           ___,            ___,            ___
),
```

### Example 3: Custom RGB Values

If you need a color not in the constants, you can still use direct RGB values:

```c
[MY_LAYER] = LED_LAYOUT(
    {128, 64, 200}, ___,  ___,  ___,  ___,  {255, 128, 0}, ___,    ___,  ___,  ___,  ___,  ___,  ___,  ___,
    // ... rest of the layout
),
```

## Adding a New Color Constant

To add a new color constant, add it near the top of keymap.c:

```c
#define RGB_MY_COLOR 123, 45, 67
```

Then use it in LED_LAYOUT:

```c
[LAYER] = LED_LAYOUT(
    {RGB_MY_COLOR}, ___,  ___,  // ...
)
```

## Tips

1. **Use `___` for off keys** - Much cleaner than writing `{RGB_OFF}` repeatedly
2. **Visual alignment** - The LED_LAYOUT matches the physical keyboard layout, making it easy to see which key is which
3. **Shorthand is your friend** - `___` saves typing and makes the layout more readable
4. **Test incrementally** - Add a few keys at a time and test to ensure they work as expected
5. **Match the keymap** - The LED_LAYOUT uses the same visual structure as the keymap's LAYOUT macro

## Benefits of This Approach

- ✅ **Visual clarity**: See the keyboard layout directly in the code - matches the physical keyboard
- ✅ **Easy modifications**: Change a color by editing one position in the visual layout
- ✅ **Named colors**: Use descriptive names instead of RGB values
- ✅ **Less repetition**: `___` shorthand for off keys
- ✅ **Consistency**: Same visual structure as the keymap's LAYOUT macro
- ✅ **Intuitive**: No need to remember LED index numbers - just position in the layout
