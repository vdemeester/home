# syntax=docker/dockerfile:1.17.0
FROM zmkfirmware/zmk-build-arm:stable AS builder

COPY ./config/west.yml /workspace/zmk-config/config/
WORKDIR /workspace/zmk-config/

# Set up the west workspace
# Initialize west, pointing to the copied zmk-config as the manifest path.
# This assumes zmk-config contains a west.yml that pulls in the main ZMK repo.
RUN west init -l config
RUN west update
RUN west zephyr-export

COPY ./config/* /workspace/zmk-config/config/
COPY . /workspace/zmk-new_corne

FROM builder AS builder_left
# Build the firmware for Corne Left
# The --build-dir ensures output is isolated for this specific build.
# We explicitly go into zmk-config to run the build command relative to it.
RUN west build -s zmk/app \
    -b eyelash_corne_left -d /tmp/left -- \
    -DZMK_CONFIG=/workspace/zmk-config/config \
    -DSHIELD=nice_view \
    -DZMK_EXTRA_MODULES=/workspace/zmk-new_corne

FROM builder AS builder_right
# Build the firmware for Corne Right
# Same as above, ensuring isolated output.
RUN west build -s zmk/app \
    -b eyelash_corne_right -d /tmp/right -- \
    -DZMK_CONFIG=/workspace/zmk-config/config \
    -DSHIELD=nice_view \
    -DZMK_EXTRA_MODULES=/workspace/zmk-new_corne

FROM scratch AS output_collector
# No actual operations here, as 'docker buildx build --output' handles the extraction.
# This stage just serves as a clear endpoint if you were to define explicit COPY --from.
COPY --from=builder_right /tmp/right/zephyr/zmk.uf2 eyelash_corne_right.uf2
COPY --from=builder_left /tmp/left/zephyr/zmk.uf2 eyelash_corne_left.uf2
