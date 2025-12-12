package main

import (
	"fmt"
	"os"
	"os/exec"
	"strings"

	"github.com/spf13/cobra"
	"github.com/vdemeester/home/tools/gh-pr/internal/output"
	"github.com/vdemeester/home/tools/gh-pr/internal/templates"
)

func createCmd(out *output.Writer) *cobra.Command {
	var (
		title     string
		body      string
		template  string
		draft     bool
		base      string
		head      string
		web       bool
		reviewers []string
		assignees []string
		labels    []string
		refresh   bool
	)

	cmd := &cobra.Command{
		Use:   "create",
		Short: "Create a pull request",
		Long: `Create a pull request with optional template support.

Templates are automatically discovered from:
  - .github/PULL_REQUEST_TEMPLATE.md
  - .github/PULL_REQUEST_TEMPLATE/
  - docs/PULL_REQUEST_TEMPLATE.md

Use --template to specify a template file, or list available templates
with 'gh-pr list-templates'.`,
		RunE: func(cmd *cobra.Command, args []string) error {
			return runCreate(out, createOpts{
				title:     title,
				body:      body,
				template:  template,
				draft:     draft,
				base:      base,
				head:      head,
				web:       web,
				reviewers: reviewers,
				assignees: assignees,
				labels:    labels,
				refresh:   refresh,
			})
		},
	}

	cmd.Flags().StringVarP(&title, "title", "t", "", "Pull request title")
	cmd.Flags().StringVarP(&body, "body", "b", "", "Pull request body")
	cmd.Flags().StringVar(&template, "template", "", "Use a specific template file")
	cmd.Flags().BoolVarP(&draft, "draft", "d", false, "Create as draft pull request")
	cmd.Flags().StringVar(&base, "base", "", "Base branch (default: main/master)")
	cmd.Flags().StringVar(&head, "head", "", "Head branch (default: current branch)")
	cmd.Flags().BoolVarP(&web, "web", "w", false, "Open in web browser")
	cmd.Flags().StringSliceVarP(&reviewers, "reviewer", "r", nil, "Request reviewers (comma-separated)")
	cmd.Flags().StringSliceVarP(&assignees, "assignee", "a", nil, "Assign users (comma-separated)")
	cmd.Flags().StringSliceVarP(&labels, "label", "l", nil, "Add labels (comma-separated)")
	cmd.Flags().BoolVar(&refresh, "refresh", false, "Refresh template cache")

	return cmd
}

type createOpts struct {
	title     string
	body      string
	template  string
	draft     bool
	base      string
	head      string
	web       bool
	reviewers []string
	assignees []string
	labels    []string
	refresh   bool
}

func runCreate(out *output.Writer, opts createOpts) error {
	// If template is specified, load it
	if opts.template != "" {
		content, err := loadTemplate(out, opts.template, opts.refresh)
		if err != nil {
			return err
		}

		// Use template content if body is empty
		if opts.body == "" {
			opts.body = content
		}
	}

	// Build gh pr create command
	ghArgs := []string{"pr", "create"}

	if opts.title != "" {
		ghArgs = append(ghArgs, "--title", opts.title)
	}

	if opts.body != "" {
		ghArgs = append(ghArgs, "--body", opts.body)
	}

	if opts.draft {
		ghArgs = append(ghArgs, "--draft")
	}

	if opts.base != "" {
		ghArgs = append(ghArgs, "--base", opts.base)
	}

	if opts.head != "" {
		ghArgs = append(ghArgs, "--head", opts.head)
	}

	if opts.web {
		ghArgs = append(ghArgs, "--web")
	}

	for _, reviewer := range opts.reviewers {
		ghArgs = append(ghArgs, "--reviewer", reviewer)
	}

	for _, assignee := range opts.assignees {
		ghArgs = append(ghArgs, "--assignee", assignee)
	}

	for _, label := range opts.labels {
		ghArgs = append(ghArgs, "--label", label)
	}

	out.Info("Creating pull request...")

	// Execute gh command
	cmd := exec.Command("gh", ghArgs...)
	cmd.Stdout = os.Stdout
	cmd.Stderr = os.Stderr
	cmd.Stdin = os.Stdin

	if err := cmd.Run(); err != nil {
		return fmt.Errorf("gh pr create failed: %w", err)
	}

	return nil
}

func loadTemplate(out *output.Writer, templatePath string, refresh bool) (string, error) {
	finder, err := templates.NewFinder()
	if err != nil {
		return "", err
	}

	// If template path is just a name, try to find it
	if !strings.Contains(templatePath, "/") {
		out.Info("Searching for template: %s", templatePath)

		tmplList, err := finder.Find(refresh)
		if err != nil {
			return "", fmt.Errorf("failed to find templates: %w", err)
		}

		for _, tmpl := range tmplList {
			if tmpl.Name == templatePath || tmpl.Path == templatePath {
				out.Success("Found template: %s", tmpl.Path)
				return tmpl.Content, nil
			}
		}

		return "", fmt.Errorf("template not found: %s", templatePath)
	}

	// Direct file path
	return templates.ReadTemplate(templatePath)
}
