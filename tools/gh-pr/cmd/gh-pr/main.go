package main

import (
	"fmt"
	"os"

	"github.com/spf13/cobra"
	"github.com/vdemeester/home/tools/gh-pr/internal/output"
)

var version = "0.1.0"

func main() {
	if err := rootCmd().Execute(); err != nil {
		fmt.Fprintf(os.Stderr, "Error: %v\n", err)
		os.Exit(1)
	}
}

func rootCmd() *cobra.Command {
	out := output.Default()

	cmd := &cobra.Command{
		Use:   "gh-pr",
		Short: "GitHub Pull Request management tool",
		Long: `A comprehensive tool for managing GitHub pull requests.

Combines PR creation with template support, workflow management,
and conflict resolution in a single command-line interface.`,
		SilenceUsage:  true,
		SilenceErrors: true,
	}

	cmd.AddCommand(versionCmd())
	cmd.AddCommand(createCmd(out))
	cmd.AddCommand(listTemplatesCmd(out))
	cmd.AddCommand(restartFailedCmd(out))
	cmd.AddCommand(resolveConflictsCmd(out))
	cmd.AddCommand(commentCmd(out))

	return cmd
}

func versionCmd() *cobra.Command {
	return &cobra.Command{
		Use:   "version",
		Short: "Print version information",
		Run: func(cmd *cobra.Command, args []string) {
			fmt.Printf("gh-pr version %s\n", version)
		},
	}
}
