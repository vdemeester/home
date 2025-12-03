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
claude-skills : ~/.claude/skills

all += claude-agents
claude-agents : ~/.claude/agents

all += claude-hooks
claude-hooks : ~/.claude/hooks

all += claude-settings
claude-settings : ~/.claude/settings.json

# Example: Override default rule to generate content instead of copying
# Uncomment and customize this pattern for files that should be generated:
#
# ~/.config/example/generated.conf : force
# 	@echo "⚙️  Generating $$@"
# 	@mkdir -p $(@D)
# 	@echo "# Generated on $$(date)" > $@
# 	@echo "setting1=value1" >> $@
# 	@echo "setting2=value2" >> $@

all : $(all)
	@echo "✅ All dotfiles installed!"

.PHONY: all $(all)
.DEFAULT_GOAL := all

