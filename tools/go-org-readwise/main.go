package main

import (
	"context"
	"flag"
	"fmt"
	"log"
	"os"
	"path/filepath"
	"time"

	"github.com/vdemeester/home/tools/go-org-readwise/internal/readwise"
)

func main() {
	apiKeyFile := flag.String("apiKeyFile", "", "File to load the apiKey from. If empty, it will defer to the READWISE_KEY environment variable")
	targetFolder := flag.String("targetFolder", "", "Folder to write highlights (in org file) into")
	flag.Parse()

	if *targetFolder == "" {
		log.Fatal("-targetFolder is a required flag")
	}

	apiKeyData, err := os.ReadFile(*apiKeyFile)
	if err != nil && !os.IsNotExist(err) {
		log.Fatalf("Error reading apiKeyFile %s: %v", *apiKeyFile, err)
	}
	apikey := string(apiKeyData)
	if apikey == "" {
		apikey = os.Getenv("READWISE_KEY")
	}

	stateFile := filepath.Join(*targetFolder, ".readwise-sync.state")
	updateAfter, err := getUpdateAfterFromFile(stateFile)
	if err != nil {
		log.Fatalf("Error reading readwise state file from %s: %v", stateFile, err)
	}
	fmt.Println(*targetFolder)
	fmt.Println("updateAfter", updateAfter)
	ctx := context.Background()
	highlights, err := readwise.FetchFromAPI(ctx, apikey, updateAfter)
	if err != nil {
		log.Fatalf("Error while fetching highlights: %v", err)
	}
	// if err := os.WriteFile(stateFile, []byte(time.Now().Format(readwise.FormatUpdatedAfter)), 0o666); err != nil {
	// 	log.Fatalf("Error writing readwise state file in %s: %v", stateFile, err)
	// }
	fmt.Println("size", len(highlights))

	// updateAfter := time.Now().Add(-72 * time.Hour)
	// fmt.Println("updateAfter:", updateAfter)
	// mhighlights, merr := readwise.FetchFromAPI(ctx, os.Getenv("READWISE_KEY"), &updateAfter)
	// if merr != nil {
	// 	fmt.Fprintf(os.Stderr, "%v\n", merr)
	// 	os.Exit(1)
	// }
	// fmt.Println("size", len(mhighlights))
	// for _, h := range highlights {
	// 	fmt.Println("title", h.Title, len(h.Highlights), h.BookTags)
	// 	// for _, hh := range h.Highlights {
	// 	// 	fmt.Println(">>>", hh.ID, hh.Tags)
	// 	// }
	// }
}

func getUpdateAfterFromFile(stateFile string) (*time.Time, error) {
	data, err := os.ReadFile(stateFile)
	if err != nil && !os.IsNotExist(err) {
		return nil, err
	}
	// If the file doesn't exists, do not fail
	if os.IsNotExist(err) {
		return nil, nil
	}
	t, err := time.Parse(readwise.FormatUpdatedAfter, string(data))
	if err != nil {
		return nil, err
	}
	return &t, nil
}
