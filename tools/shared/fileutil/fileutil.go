package fileutil

import (
	"fmt"
	"io/ioutil"
	"os"
	"strconv"
	"strings"
)

// ReadFileContentTrimmed reads a file and returns the trimmed content.
// It handles the common pattern of reading a file, checking for errors,
// and trimming whitespace from the content.
func ReadFileContentTrimmed(filePath string) (string, error) {
	content, err := ioutil.ReadFile(filePath)
	if err != nil {
		return "", fmt.Errorf("failed to read file %s: %w", filePath, err)
	}
	return strings.TrimSpace(string(content)), nil
}

// ReadFileContentTrimmedWithDefault reads a file and returns the trimmed content.
// If the file doesn't exist, it returns the default value instead of an error.
func ReadFileContentTrimmedWithDefault(filePath string, defaultValue string) (string, error) {
	content, err := ioutil.ReadFile(filePath)
	if err != nil {
		if os.IsNotExist(err) {
			return defaultValue, nil
		}
		return "", fmt.Errorf("failed to read file %s: %w", filePath, err)
	}
	return strings.TrimSpace(string(content)), nil
}

// ReadFileContentTrimmedOptional reads a file and returns the trimmed content.
// If the file doesn't exist, it returns an empty string without error.
// For other errors, it returns the error.
func ReadFileContentTrimmedOptional(filePath string) (string, error) {
	content, err := ioutil.ReadFile(filePath)
	if err != nil {
		if os.IsNotExist(err) {
			return "", nil
		}
		return "", fmt.Errorf("failed to read file %s: %w", filePath, err)
	}
	return strings.TrimSpace(string(content)), nil
}

// ReadIntFromFile reads a file and parses its trimmed content as an integer.
func ReadIntFromFile(filePath string) (int, error) {
	content, err := ReadFileContentTrimmed(filePath)
	if err != nil {
		return 0, err
	}
	value, err := strconv.Atoi(content)
	if err != nil {
		return 0, fmt.Errorf("failed to parse content '%s' as integer from file %s: %w", content, filePath, err)
	}
	return value, nil
}