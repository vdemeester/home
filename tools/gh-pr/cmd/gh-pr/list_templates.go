package main

import (
	"fmt"

	"github.com/spf13/cobra"
	"github.com/vdemeester/home/tools/gh-pr/internal/output"
	"github.com/vdemeester/home/tools/gh-pr/internal/templates"
)

func listTemplatesCmd(out *output.Writer) *cobra.Command {
	var (
		refresh bool
		verbose bool
	)

	cmd := &cobra.Command{
		Use:   "list-templates",
		Short: "List available pull request templates",
		Long: `List all pull request templates found in the repository.

Templates are cached for one week by default. Use --refresh to bypass
the cache and search for templates again.`,
		RunE: func(cmd *cobra.Command, args []string) error {
			return runListTemplates(out, refresh, verbose)
		},
	}

	cmd.Flags().BoolVar(&refresh, "refresh", false, "Refresh template cache")
	cmd.Flags().BoolVarP(&verbose, "verbose", "v", false, "Show template content preview")

	return cmd
}

func runListTemplates(out *output.Writer, refresh, verbose bool) error {
	finder, err := templates.NewFinder()
	if err != nil {
		return fmt.Errorf("failed to create template finder: %w", err)
	}

	if refresh {
		out.Info("Refreshing template cache...")
	}

	tmplList, err := finder.Find(refresh)
	if err != nil {
		return fmt.Errorf("failed to find templates: %w", err)
	}

	if len(tmplList) == 0 {
		out.Warning("No pull request templates found.")
		out.Println("")
		out.Println("Templates are typically located in:")
		out.Println("  - .github/PULL_REQUEST_TEMPLATE.md")
		out.Println("  - .github/PULL_REQUEST_TEMPLATE/")
		out.Println("  - docs/PULL_REQUEST_TEMPLATE.md")
		return nil
	}

	out.Success("Found %d pull request template(s):", len(tmplList))
	out.Println("")

	for i, tmpl := range tmplList {
		out.Println("%d. %s", i+1, tmpl.Name)
		out.Println("   Path: %s", tmpl.Path)

		if verbose {
			// Show first few lines of template
			lines := splitLines(tmpl.Content, 5)
			out.Println("   Preview:")
			for _, line := range lines {
				out.Println("     %s", line)
			}
			if len(lines) == 5 {
				out.Println("     ...")
			}
		}

		if i < len(tmplList)-1 {
			out.Println("")
		}
	}

	out.Println("")
	out.Info("Use 'gh-pr create --template <name>' to create a PR with a template")

	return nil
}

func splitLines(content string, max int) []string {
	lines := []string{}
	current := ""

	for i, char := range content {
		if char == '\n' {
			lines = append(lines, current)
			current = ""

			if len(lines) >= max {
				break
			}
		} else {
			current += string(char)
		}

		// Handle last line
		if i == len(content)-1 && current != "" {
			lines = append(lines, current)
		}
	}

	return lines
}
