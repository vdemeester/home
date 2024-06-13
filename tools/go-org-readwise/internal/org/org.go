package org

import (
	"context"
	"errors"
	"fmt"
	"os"
	"regexp"
	"strings"

	"github.com/vdemeester/home/tools/go-org-readwise/internal/readwise"
)

const (
	// denote-id-format "%Y%m%dT%H%M%S"
	denoteDateFormat = "20060102T150405"
	// punctionation that is removed from file names.
	denoteExcludedPunctuationRegexpStr = "[][{}!@#$%^&*()=+'\"?,.|;:~`‘’“”/]*"
)

var (
	denoteExcludedPunctuationRegexp = regexp.MustCompile(denoteExcludedPunctuationRegexpStr)
	replaceHypensRegexp             = regexp.MustCompile("[-]+")
)

/*
For each results:
- Define a filename (denote naming — gonna be weird but meh) — from title + first highlight date
- Detect if the file exists
- If the file doesn't exist, create the file
- If the file exist, append

For the file format: org file with denote naming
And use the update date to add new highlights
*/

func Sync(ctx context.Context, target string, results []readwise.Result) error {
	for _, result := range results {
		// FIXME: handle the case where tags where added after
		// a sync. In that case, we want to try different
		// titles (without tags, …) ; most likely we want to
		// use a regexp to "detect" part of the thing.
		filename := denoteFilename(result)
		fmt.Println("file", filename)
		if _, err := os.Stat(filename); err == nil {
			// Append to the file
			return errors.New("Not implemented")
		} else if errors.Is(err, os.ErrNotExist) {
			// Create the file
		} else {
			// Schrodinger: file may or may not exist. See err for details.
			// Therefore, do *NOT* use !os.IsNotExist(err) to test for file existence
			return err
		}
	}
	return nil
}

// See https://protesilaos.com/emacs/denote#h:4e9c7512-84dc-4dfb-9fa9-e15d51178e5d
// DATE==SIGNATURE--TITLE__KEYWORDS.EXTENSION
// Examples:
// - 20240611T100401--tuesday-11-june-2024__journal.org
// - 20240511T100401==readwise--foo__bar_baz.org
func denoteFilename(result readwise.Result) string {
	var date, signature, title, keywords string
	// The DATE field represents the date in year-month-day format
	// followed by the capital letter T (for “time”) and the
	// current time in hour-minute-second notation. The
	// presentation is compact: 20220531T091625. The DATE serves
	// as the unique identifier of each note and, as such, is also
	// known as the file’s ID or identifier.
	date = result.FirstHighlightDate().Format(denoteDateFormat)

	// File names can include a string of alphanumeric characters
	// in the SIGNATURE field. Signatures have no clearly defined
	// purpose and are up to the user to define. One use-case is
	// to use them to establish sequential relations between files
	// (e.g. 1, 1a, 1b, 1b1, 1b2, …).
	// We use signature to mark files synced from readwise.
	signature = "==readwise"

	// The TITLE field is the title of the note, as provided by
	// the user. It automatically gets downcased by default and is
	// also hyphenated (Sluggification of file name
	// components). An entry about “Economics in the Euro Area”
	// produces an economics-in-the-euro-area string for the TITLE
	// of the file name.
	title = sluggify(result.Title)

	// The KEYWORDS field consists of one or more entries
	// demarcated by an underscore (the separator is inserted
	// automatically). Each keyword is a string provided by the
	// user at the relevant prompt which broadly describes the
	// contents of the entry.
	if len(result.BookTags) > 0 {
		tags := make([]string, len(result.BookTags))
		for i, t := range result.BookTags {
			tags[i] = sluggify(t.Name)
		}
		keywords = "__" + strings.Join(tags, "_")
	}

	return fmt.Sprintf("%s%s--%s%s.org", date, signature, title, keywords)
}

func sluggify(s string) string {
	// Remove punctuation
	s = denoteExcludedPunctuationRegexp.ReplaceAllString(s, "")
	// Replace spaces with hypens
	s = strings.ReplaceAll(s, " ", "-")
	// Replace underscore with hypens
	s = strings.ReplaceAll(s, "_", "-")
	// Replace multiple hypens with a single one
	s = replaceHypensRegexp.ReplaceAllString(s, "-")
	// Remove any leading and trailing hypen
	s = strings.TrimPrefix(s, "-")
	s = strings.TrimSuffix(s, "-")
	return s
}
