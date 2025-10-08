# Quick Start: Modifying LED Colors

This guide shows how to quickly modify LED colors using the visual LED_LAYOUT macro.

## Example 1: Change a Single Key Color

**Task:** Change the top-left key (position 0,0) from OFF to RED in the QWER layer.

**Solution:**
Just edit the LED_LAYOUT - find the first position and change it:
```c
[QWER] = LED_LAYOUT(
    {RGB_RED},       {RGB_ORANGE_YELLOW}, ...  // Changed from ___
```

The visual layout makes it obvious which key you're changing!

## Example 2: Change All Number Keys to a Different Color

**Task:** Change all number keys from YELLOW to GREEN in NUMB layer.

**Solution:**
Find the number keys in the visual layout and update them:
```c
[NUMB] = LED_LAYOUT(
    {RGB_RED},       ___,        ___,        ___,        ___,        ___,        ___,                           ___,        ___,        ___,        ___,        ___,        ___,        ___,
    ___,             {RGB_BLUE}, {RGB_BLUE}, {RGB_BLUE}, {RGB_BLUE}, {RGB_BLUE}, ___,                           ___,        ___,        ___,        {RGB_GREEN}, {RGB_GREEN}, {RGB_GREEN}, {RGB_YELLOW_GREEN},  // Changed
    ___,             {RGB_BLUE}, {RGB_BLUE}, {RGB_BLUE}, {RGB_BLUE}, {RGB_BLUE}, ___,                           ___,        ___,        ___,        {RGB_GREEN}, {RGB_GREEN}, {RGB_GREEN}, {RGB_YELLOW_GREEN},  // Changed
    ___,             {RGB_BLUE}, {RGB_BLUE}, {RGB_BLUE_BRIGHT}, {RGB_BLUE_BRIGHT}, {RGB_BLUE_BRIGHT},                                   ___,        ___,        {RGB_GREEN}, {RGB_GREEN}, {RGB_GREEN}, {RGB_YELLOW_GREEN},  // Changed
    // ...
)
```

## Example 3: Add a New Color Constant

**Task:** Add a custom pink color.

1. Add the constant at the top of keymap.c:
```c
#define RGB_PINK 255, 105, 180
```

2. Use it in LED_LAYOUT:
```c
[SYMB] = LED_LAYOUT(
    ___,        ___,        {RGB_PINK},  {RGB_PINK}, ...
)
```

## Example 4: Add LED Configuration for MOUS Layer

**Task:** Add blue highlighting for mouse buttons in the MOUS layer.

Add a new layer definition using LED_LAYOUT:

```c
[MOUS] = LED_LAYOUT(
    ___,             ___,             ___,             ___,             ___,             ___,             ___,                           ___,             ___,             ___,             ___,             ___,             ___,             ___,
    ___,             ___,             ___,             ___,             ___,             ___,             ___,                           ___,             ___,             {RGB_CYAN},      ___,             ___,             ___,             ___,
    ___,             ___,             ___,             ___,             ___,             ___,             ___,                           ___,             ___,             {RGB_CYAN},      {RGB_CYAN},      ___,             ___,             ___,
    ___,             ___,             ___,             ___,             ___,             ___,                                                             ___,             {RGB_CYAN},      ___,             ___,             ___,             ___,
    ___,             ___,             ___,             ___,             ___,                              ___,                           ___,                              ___,             ___,             ___,             ___,             ___,
                                                                        {RGB_BLUE_LIGHT}, {RGB_BLUE_BRIGHT}, ___,                           ___,             {RGB_BLUE_BRIGHT}, {RGB_BLUE_LIGHT}
),
```

Then update the rgb_matrix_indicators_user function to include MOUS.

## Example 5: Create a Visual Pattern

**Task:** Make the home row keys gradient from blue to green in QWER layer.

The visual layout makes this incredibly easy - just edit the home row (row 3):

```c
[QWER] = LED_LAYOUT(
    ___,                 {RGB_ORANGE_YELLOW}, {RGB_ORANGE_YELLOW}, {RGB_ORANGE_YELLOW}, {RGB_ORANGE_YELLOW}, {RGB_ORANGE_YELLOW}, ___,                           ___,             {RGB_ORANGE_YELLOW}, {RGB_ORANGE_YELLOW}, {RGB_ORANGE_YELLOW}, {RGB_ORANGE_YELLOW}, {RGB_ORANGE_YELLOW}, {RGB_ORANGE_YELLOW},
    {RGB_ORANGE_YELLOW}, {RGB_ORANGE_YELLOW}, {RGB_ORANGE_YELLOW}, {RGB_ORANGE_YELLOW}, {RGB_ORANGE_YELLOW}, {RGB_ORANGE_YELLOW}, ___,                           ___,             {RGB_ORANGE_YELLOW}, {RGB_ORANGE_YELLOW}, {RGB_ORANGE_YELLOW}, {RGB_ORANGE_YELLOW}, {RGB_ORANGE_YELLOW}, {RGB_ORANGE_YELLOW},
    {RGB_GREEN},         {0, 100, 255},       {0, 150, 200},       {0, 200, 150},       {0, 255, 100},       {RGB_GREEN},         ___,                           ___,             {RGB_GREEN},         {0, 255, 100},       {0, 200, 150},       {0, 150, 200},       {0, 100, 255},       {RGB_GREEN},
    // ... rest remains the same
)
```

Notice how you can immediately see which keys you're modifying!

## Tips

1. **Visual representation** - LED_LAYOUT matches the physical keyboard layout exactly
2. **Use `___` shorthand** - Much cleaner than `{RGB_OFF}` everywhere
3. **Test incrementally** - Change a few keys at a time and build/flash
4. **Use named colors** - Prefer RGB_BLUE over `{0, 55, 255}` for clarity
5. **Easy to spot mistakes** - With the visual layout, you can immediately see if a key is in the wrong position

## Comparison to Old Approach

**Old way (array indices):**
```c
[LAYER] = {
    [0] = {RGB_RED}, [5] = {RGB_BLUE}, [10] = {RGB_GREEN}, ...
}
```
❌ Hard to visualize which physical key you're modifying

**New way (LED_LAYOUT):**
```c
[LAYER] = LED_LAYOUT(
    {RGB_RED}, {RGB_BLUE}, {RGB_GREEN}, ...
)
```
✅ Visual representation matches the physical keyboard!

## LED Layout Reference (Visual)

The LED_LAYOUT macro follows this structure (matches keyboard physical layout):

```
Row 0:  k00, k05, k10, k15, k20, k25, k29        k65, k61, k56, k51, k46, k41, k36
Row 1:  k01, k06, k11, k16, k21, k26, k30        k66, k62, k57, k52, k47, k42, k37
Row 2:  k02, k07, k12, k17, k22, k27, k31        k67, k63, k58, k53, k48, k43, k38
Row 3:  k03, k08, k13, k18, k23, k28                  k64, k59, k54, k49, k44, k39
Row 4:  k04, k09, k14, k19, k24,      k35        k71,      k60, k55, k50, k45, k40
Thumb:                      k32, k33, k34        k68, k69, k70
```

## Available Colors

```c
RGB_OFF, RGB_RED, RGB_ORANGE_YELLOW, RGB_GREEN, RGB_GREEN_LIGHT,
RGB_CYAN, RGB_BLUE_LIGHT, RGB_BLUE, RGB_BLUE_BRIGHT, RGB_WHITE,
RGB_YELLOW, RGB_YELLOW_GREEN, RGB_PURPLE, RGB_GREEN_MEDIA, RGB_PURPLE_VOL
```
