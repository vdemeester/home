makefile := $(abspath $(lastword $(MAKEFILE_LIST)))
dotfiles := $(abspath $(dir $(makefile)))

~ := $(abspath $(dotfiles))

force:

define rule.template
$(1)/% : $(2)/% force
	@echo "📋 Linking $$< → $$@"
	@mkdir -p $$(@D)
	@ln -snf $$< $$@
endef

rule.define = $(eval $(call rule.template,$(1),$(2)))

$(call rule.define,~,$(~))

all += niri
niri : ~/.config/niri/config.kdl

all += claude-skills
claude-skills : ~/.config/claude/skills

all += claude-agents claude-settings claude-plugins claude-statusline claude-compat
claude-agents : ~/.config/claude/agents
claude-settings : ~/.config/claude/settings.json
claude-plugins : ~/.config/claude/plugins/session-manager
claude-statusline : ~/.config/claude/statusline.sh
claude-compat : ~/.claude

all += ntfy-config
ntfy-config : ~/.config/ntfy/client.yml

# Backward compatibility: symlink ~/.claude to ~/.config/claude
~/.claude : force
	@echo "🔗 Creating backward compatibility symlink: ~/.claude -> ~/.config/claude"
	@mkdir -p ~/.config
	@ln -snf ~/.config/claude ~/.claude

# Generate ntfy client.yml from template with passage secrets injected
~/.config/ntfy/client.yml : $(dotfiles)/.config/ntfy/client.yml.in $(dotfiles)/.config/ntfy/ntfy-update-config force
	@echo "⚙️  Generating $$@ from template with passage secrets"
	@$(dotfiles)/.config/ntfy/ntfy-update-config

all : $(all)
	@echo "✅ All dotfiles installed!"

.PHONY: all $(all)
.DEFAULT_GOAL := all

