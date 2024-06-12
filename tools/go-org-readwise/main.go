package main

import (
	"context"
	"fmt"
	"os"
	"time"

	"github.com/vdemeester/home/tools/go-org-readwise/internal/readwise"
)

func main() {
	ctx := context.Background()
	highlights, merr := readwise.FetchFromAPI(ctx, os.Getenv("READWISE_KEY"), nil)
	if merr != nil {
		fmt.Fprintf(os.Stderr, "%v\n", merr)
		os.Exit(1)
	}
	fmt.Println("count", highlights.Count)
	fmt.Println("nextPageCursor", *highlights.NextPageCursor)
	fmt.Println("size", len(highlights.Results))

	updateAfter := time.Now().Add(-1000 * time.Hour)
	fmt.Println("updateAfter:", updateAfter)
	highlights, merr = readwise.FetchFromAPI(ctx, os.Getenv("READWISE_KEY"), &updateAfter)
	if merr != nil {
		fmt.Fprintf(os.Stderr, "%v\n", merr)
		os.Exit(1)
	}
	fmt.Println("count", highlights.Count)
	fmt.Println("nextPageCursor", *highlights.NextPageCursor)
	fmt.Println("size", len(highlights.Results))
	for _, h := range highlights.Results {
		fmt.Println("title", h.Title, len(h.Highlights), h.BookTags)
		// for _, hh := range h.Highlights {
		// 	fmt.Println(">>>", hh.ID, hh.Tags)
		// }
	}
}
