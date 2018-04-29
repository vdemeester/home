{ configs, pkgs, ...}:

{
  programs.fish = {
    enable = true;
  };
  environment.etc."fish/functions/fish_prompt.fish".source = ../envs/fish/fish_prompt.fish;
  environment.etc."fish/functions/fish_right_prompt.fish".source = ../envs/fish/fish_right_prompt.fish;
}
