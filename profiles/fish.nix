{ configs, pkgs, ...}:

{
  programs.fish = {
    enable = true;
    promptInit = ''
      source /etc/fish/functions/fish_prompt.fish
      source /etc/fish/functions/fish_right_prompt.fish
    '';
  };
  environment.etc."fish/functions/fish_prompt.fish".source = ../envs/fish/fish_prompt.fish;
  environment.etc."fish/functions/fish_right_prompt.fish".source = ../envs/fish/fish_right_prompt.fish;
}
