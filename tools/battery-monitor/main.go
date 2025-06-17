package main

import (
	"flag"
	"fmt"
	"io/ioutil"
	"log"
	"os"
	"os/exec"
	"path/filepath"
	"strconv"
	"strings"

	"github.com/fsnotify/fsnotify"
)

func main() {
	// 1. Define command-line flags for configuration
	batteryPath := flag.String("battery-path", "/sys/class/power_supply/BAT0", "Path to the battery status directory (e.g., /sys/class/power_supply/BAT0)")
	lowThreshold := flag.Int("low-threshold", 40, "Battery percentage threshold for power-saver profile when on battery")
	onPowerProfile := flag.String("on-power-profile", "performance", "Power profile to set when on AC power")
	onBatteryBalancedProfile := flag.String("on-battery-balanced-profile", "balanced", "Power profile to set when on battery and above low threshold")
	onBatteryLowProfile := flag.String("on-battery-low-profile", "power-saver", "Power profile to set when on battery and below low threshold")
	enableNotifications := flag.Bool("enable-notifications", true, "Enable desktop notifications using notify-send")
	notificationIcon := flag.String("notification-icon", "battery", "Icon name for desktop notifications (e.g., 'battery', 'dialog-information')")

	flag.Parse()

	log.Printf("Starting battery monitor with settings:")
	log.Printf("  Battery Path: %s", *batteryPath)
	log.Printf("  Low Threshold (on battery): %d%%", *lowThreshold)
	log.Printf("  On AC Power Profile: %s", *onPowerProfile)
	log.Printf("  On Battery Balanced Profile: %s", *onBatteryBalancedProfile)
	log.Printf("  On Battery Low Profile: %s", *onBatteryLowProfile)
	log.Printf("  Desktop Notifications Enabled: %t", *enableNotifications)
	if *enableNotifications {
		log.Printf("  Notification Icon: %s", *notificationIcon)
	}

	// Determine the full paths to relevant files
	capacityFilePath := filepath.Join(*batteryPath, "capacity")
	statusFilePath := filepath.Join(*batteryPath, "status") // Typically "Charging" or "Discharging"

	// Ensure the battery paths exist
	if _, err := os.Stat(capacityFilePath); os.IsNotExist(err) {
		log.Fatalf("Error: Battery capacity file not found at %s. Please check --battery-path.", capacityFilePath)
	}
	if _, err := os.Stat(statusFilePath); os.IsNotExist(err) {
		log.Fatalf("Error: Battery status file not found at %s. Please check --battery-path.", statusFilePath)
	}

	currentProfile := "" // To keep track of the currently set profile

	// Initial check on startup
	log.Println("Performing initial battery status check...")
	status, capacity, err := readBatteryStatusAndCapacity(statusFilePath, capacityFilePath)
	if err != nil {
		log.Printf("Initial check failed: %v. Retrying on file change.", err)
	} else {
		// Pass notification settings to the apply function
		currentProfile = applyPowerProfile(status, capacity, *lowThreshold, *onPowerProfile, *onBatteryBalancedProfile, *onBatteryLowProfile, currentProfile, *enableNotifications, *notificationIcon)
	}

	// Create a new watcher.
	watcher, err := fsnotify.NewWatcher()
	if err != nil {
		log.Fatal("Error creating watcher:", err)
	}
	defer watcher.Close()

	done := make(chan bool)

	go func() {
		for {
			select {
			case event, ok := <-watcher.Events:
				if !ok {
					return
				}
				if event.Op&fsnotify.Write == fsnotify.Write {
					log.Printf("File modified: %s - Event: %s", event.Name, event.Op.String())

					// Read status and capacity on change
					status, capacity, err := readBatteryStatusAndCapacity(statusFilePath, capacityFilePath)
					if err != nil {
						log.Printf("Error reading battery status/capacity: %v", err)
						continue
					}
					// Pass notification settings to the apply function
					currentProfile = applyPowerProfile(status, capacity, *lowThreshold, *onPowerProfile, *onBatteryBalancedProfile, *onBatteryLowProfile, currentProfile, *enableNotifications, *notificationIcon)
				}
			case err, ok := <-watcher.Errors:
				if !ok {
					return
				}
				log.Println("Error from watcher:", err)
			}
		}
	}()

	err = watcher.Add(capacityFilePath)
	if err != nil {
		log.Fatalf("Error adding %s to watcher: %v", capacityFilePath, err)
	}
	log.Printf("Watching %s for changes...", capacityFilePath)

	err = watcher.Add(statusFilePath)
	if err != nil {
		log.Fatalf("Error adding %s to watcher: %v", statusFilePath, err)
	}
	log.Printf("Watching %s for changes...", statusFilePath)

	<-done // Keep the main goroutine alive
}

// readBatteryStatusAndCapacity reads the battery status (Charging/Discharging) and percentage.
func readBatteryStatusAndCapacity(statusPath, capacityPath string) (string, int, error) {
	// Read status
	statusContent, err := ioutil.ReadFile(statusPath)
	if err != nil {
		return "", 0, fmt.Errorf("failed to read battery status file %s: %w", statusPath, err)
	}
	status := strings.TrimSpace(string(statusContent))

	// Read capacity
	capacityContent, err := ioutil.ReadFile(capacityPath)
	if err != nil {
		return "", 0, fmt.Errorf("failed to read battery capacity file %s: %w", capacityPath, err)
	}
	capacityStr := strings.TrimSpace(string(capacityContent))
	capacity, err := strconv.Atoi(capacityStr)
	if err != nil {
		return "", 0, fmt.Errorf("failed to parse battery capacity '%s': %w", capacityStr, err)
	}
	return status, capacity, nil
}

// applyPowerProfile determines and sets the correct power profile and sends a notification.
// It returns the profile that was actually set (or determined to be set).
func applyPowerProfile(status string, capacity int, lowThreshold int, onPowerProfile, onBatteryBalancedProfile, onBatteryLowProfile, currentProfile string, enableNotifications bool, notificationIcon string) string {
	var newProfile string
	var notificationMessage string

	log.Printf("Current Status: %s, Capacity: %d%%", status, capacity)

	if status == "Charging" || status == "Full" {
		newProfile = onPowerProfile
		notificationMessage = fmt.Sprintf("Power connected. Switching to %s profile.", newProfile)
	} else if status == "Discharging" {
		if capacity <= lowThreshold {
			newProfile = onBatteryLowProfile
			notificationMessage = fmt.Sprintf("Battery low (%d%%). Switching to %s profile.", capacity, newProfile)
		} else {
			newProfile = onBatteryBalancedProfile
			notificationMessage = fmt.Sprintf("Battery on power (%d%%). Switching to %s profile.", capacity, newProfile)
		}
	} else {
		log.Printf("Unknown battery status: %s. Defaulting to balanced profile.", status)
		newProfile = onBatteryBalancedProfile // Fallback
		notificationMessage = fmt.Sprintf("Unknown battery status '%s'. Defaulting to %s profile.", status, newProfile)
	}

	if newProfile != currentProfile {
		log.Printf("Calculated new profile: %s (Current Status: %s, Capacity: %d%%). Attempting to set.", newProfile, status, capacity)
		err := setPowerProfile(newProfile)
		if err != nil {
			log.Printf("Error setting power profile to %s: %v", newProfile, err)
			if enableNotifications {
				sendNotification("Battery Monitor Error", fmt.Sprintf("Failed to set power profile to %s: %v", newProfile, err), "dialog-error")
			}
			return currentProfile // If setting failed, we stick to the old profile
		} else {
			log.Printf("Successfully set power profile to %s", newProfile)
			if enableNotifications {
				sendNotification("Power Profile Changed", notificationMessage, notificationIcon)
			}
			return newProfile // Update currentProfile only on success
		}
	} else {
		log.Printf("Power profile already set to %s. No change needed.", currentProfile)
		return currentProfile // Return the current profile as no change occurred
	}
}

// setPowerProfile executes the powerprofilesctl command to set the profile.
func setPowerProfile(profile string) error {
	cmd := exec.Command("powerprofilesctl", "set", profile)
	output, err := cmd.CombinedOutput()
	if err != nil {
		return fmt.Errorf("command 'powerprofilesctl set %s' failed: %w\nOutput: %s", profile, err, string(output))
	}
	log.Printf("powerprofilesctl output: %s", strings.TrimSpace(string(output)))
	return nil
}

// sendNotification executes the notify-send command.
func sendNotification(summary, body, icon string) {
	cmd := exec.Command("notify-send", "-i", icon, summary, body)
	err := cmd.Run()
	if err != nil {
		log.Printf("Error sending notification: %v (Is notify-send installed and a notification daemon running?)", err)
	}
}
