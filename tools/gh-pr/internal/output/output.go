package output

import (
	"fmt"
	"io"
	"os"
)

// Color codes for terminal output
const (
	Red    = "\033[0;31m"
	Green  = "\033[0;32m"
	Yellow = "\033[1;33m"
	Blue   = "\033[0;34m"
	Reset  = "\033[0m"
)

// Writer provides colored output methods
type Writer struct {
	out    io.Writer
	err    io.Writer
	colors bool
}

// NewWriter creates a new output writer
func NewWriter(out, err io.Writer, colors bool) *Writer {
	return &Writer{
		out:    out,
		err:    err,
		colors: colors,
	}
}

// Default creates a writer that outputs to stdout/stderr with colors
func Default() *Writer {
	return NewWriter(os.Stdout, os.Stderr, true)
}

// colorize wraps text in color codes if colors are enabled
func (w *Writer) colorize(color, text string) string {
	if !w.colors {
		return text
	}
	return color + text + Reset
}

// Info prints an informational message
func (w *Writer) Info(format string, args ...interface{}) {
	msg := fmt.Sprintf(format, args...)
	fmt.Fprintln(w.out, w.colorize(Blue, msg))
}

// Success prints a success message
func (w *Writer) Success(format string, args ...interface{}) {
	msg := fmt.Sprintf(format, args...)
	fmt.Fprintln(w.out, w.colorize(Green, msg))
}

// Warning prints a warning message
func (w *Writer) Warning(format string, args ...interface{}) {
	msg := fmt.Sprintf(format, args...)
	fmt.Fprintln(w.err, w.colorize(Yellow, msg))
}

// Error prints an error message
func (w *Writer) Error(format string, args ...interface{}) {
	msg := fmt.Sprintf(format, args...)
	fmt.Fprintln(w.err, w.colorize(Red, msg))
}

// Print prints a message without color
func (w *Writer) Print(format string, args ...interface{}) {
	fmt.Fprintf(w.out, format, args...)
}

// Println prints a message with newline without color
func (w *Writer) Println(format string, args ...interface{}) {
	msg := fmt.Sprintf(format, args...)
	fmt.Fprintln(w.out, msg)
}
