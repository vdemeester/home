package readwise

// TODO: support pages

import (
	"context"
	"encoding/json"
	"io"
	"net/http"
	"time"
)

func FetchFromAPI(ctx context.Context, apikey string, updateAfter *time.Time) (Export, error) {
	export := Export{}
	endpoint := "https://readwise.io/api/v2/export"
	if updateAfter != nil {
		endpoint = endpoint + "/?updateAfter=" + updateAfter.Format(time.RFC3339)
	}

	httpClient := &http.Client{}

	req, err := http.NewRequestWithContext(ctx, "GET", endpoint, nil)
	if err != nil {
		return export, err
	}
	req.Header.Add("Authorization", "Token "+apikey)
	resp, err := httpClient.Do(req)
	if err != nil {
		return export, err
	}

	defer resp.Body.Close()

	body, err := io.ReadAll(resp.Body)
	if err != nil {
		return export, err
	}

	err = json.Unmarshal(body, &export)
	if err != nil {
		return export, err
	}

	return export, nil
}
