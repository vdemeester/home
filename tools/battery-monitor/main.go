package main

import (
	"flag"
	"fmt"
	"log"
	"os"
	"os/exec"
	"path/filepath"
	"strings"
	"time"

	"github.com/vdemeester/home/tools/battery-monitor/internal/fileutil"
)

func main() {
	// 1. Define command-line flags for configuration
	batteryPath := flag.String("battery-path", "/sys/class/power_supply/BAT0", "Path to the battery status directory (e.g., /sys/class/power_supply/BAT0)")
	acPath := flag.String("ac-path", "/sys/class/power_supply/AC", "Path to the AC adapter status directory (e.g., /sys/class/power_supply/AC)")
	lowThreshold := flag.Int("low-threshold", 40, "Battery percentage threshold for power-saver profile when on battery")
	onPowerProfile := flag.String("on-power-profile", "performance", "Power profile to set when on AC power (regardless of charging status)")
	onBatteryBalancedProfile := flag.String("on-battery-balanced-profile", "balanced", "Power profile to set when on battery and above low threshold")
	onBatteryLowProfile := flag.String("on-battery-low-profile", "power-saver", "Power profile to set when on battery and below low threshold")
	enableNotifications := flag.Bool("enable-notifications", true, "Enable desktop notifications using notify-send")
	notificationIcon := flag.String("notification-icon", "battery", "Icon name for desktop notifications (e.g., 'battery', 'dialog-information')")
	interval := flag.Duration("interval", 10*time.Second, "Interval between battery checks (e.g., 10s, 1m)") // Reverted to polling interval

	flag.Parse()

	log.Printf("Starting battery monitor with settings:")
	log.Printf("  Battery Path: %s", *batteryPath)
	log.Printf("  AC Path: %s", *acPath)
	log.Printf("  Low Threshold (on battery): %d%%", *lowThreshold)
	log.Printf("  On AC Power Profile: %s", *onPowerProfile)
	log.Printf("  On Battery Balanced Profile: %s", *onBatteryBalancedProfile)
	log.Printf("  On Battery Low Profile: %s", *onBatteryLowProfile)
	log.Printf("  Desktop Notifications Enabled: %t", *enableNotifications)
	if *enableNotifications {
		log.Printf("  Notification Icon: %s", *notificationIcon)
	}
	log.Printf("  Check Interval: %s", *interval)

	// Determine the full paths to relevant files
	batteryCapacityFilePath := filepath.Join(*batteryPath, "capacity")
	batteryStatusFilePath := filepath.Join(*batteryPath, "status") // e.g., "Charging", "Discharging", "Full"
	acOnlineFilePath := filepath.Join(*acPath, "online")           // 0 or 1

	// Ensure the necessary paths exist
	if _, err := os.Stat(batteryCapacityFilePath); os.IsNotExist(err) {
		log.Fatalf("Error: Battery capacity file not found at %s. Please check --battery-path.", batteryCapacityFilePath)
	}
	if _, err := os.Stat(batteryStatusFilePath); os.IsNotExist(err) {
		log.Fatalf("Error: Battery status file not found at %s. Please check --battery-path.", batteryStatusFilePath)
	}
	if _, err := os.Stat(acOnlineFilePath); os.IsNotExist(err) {
		log.Fatalf("Error: AC online file not found at %s. Please check --ac-path.", acOnlineFilePath)
	}

	currentProfile := "" // To keep track of the currently set profile

	// Main monitoring loop using time.Ticker for polling
	ticker := time.NewTicker(*interval)
	defer ticker.Stop()

	// Perform initial check immediately
	log.Println("Performing initial battery status check...")
	acConnected, batteryStatus, batteryCapacity, err := readSystemStatus(acOnlineFilePath, batteryStatusFilePath, batteryCapacityFilePath)
	if err != nil {
		log.Printf("Initial check failed: %v. Will retry on next interval.", err)
	} else {
		currentProfile = applyPowerProfile(acConnected, batteryStatus, batteryCapacity, *lowThreshold, *onPowerProfile, *onBatteryBalancedProfile, *onBatteryLowProfile, currentProfile, *enableNotifications, *notificationIcon)
	}

	for range ticker.C {
		log.Println("Performing scheduled battery status check...")
		acConnected, batteryStatus, batteryCapacity, err := readSystemStatus(acOnlineFilePath, batteryStatusFilePath, batteryCapacityFilePath)
		if err != nil {
			log.Printf("Error reading system status: %v", err)
			continue
		}
		currentProfile = applyPowerProfile(acConnected, batteryStatus, batteryCapacity, *lowThreshold, *onPowerProfile, *onBatteryBalancedProfile, *onBatteryLowProfile, currentProfile, *enableNotifications, *notificationIcon)
	}
}

// readSystemStatus reads the AC online status, battery status, and battery capacity.
func readSystemStatus(acOnlinePath, batteryStatusPath, batteryCapacityPath string) (bool, string, int, error) {
	// Read AC online status
	acOnlineStr, err := fileutil.ReadFileContentTrimmed(acOnlinePath)
	if err != nil {
		return false, "", 0, err
	}
	acOnline := acOnlineStr == "1"

	// Read battery status
	batteryStatus, err := fileutil.ReadFileContentTrimmed(batteryStatusPath)
	if err != nil {
		return false, "", 0, err
	}

	// Read battery capacity
	batteryCapacity, err := fileutil.ReadIntFromFile(batteryCapacityPath)
	if err != nil {
		return false, "", 0, err
	}
	
	return acOnline, batteryStatus, batteryCapacity, nil
}

// applyPowerProfile determines and sets the correct power profile and sends a notification.
// It returns the profile that was actually set (or determined to be set).
func applyPowerProfile(acConnected bool, batteryStatus string, batteryCapacity int, lowThreshold int, onPowerProfile, onBatteryBalancedProfile, onBatteryLowProfile, currentProfile string, enableNotifications bool, notificationIcon string) string {
	var newProfile string
	var notificationMessage string

	log.Printf("Current AC Connected: %t, Battery Status: %s, Capacity: %d%%", acConnected, batteryStatus, batteryCapacity)

	if acConnected {
		newProfile = onPowerProfile
		notificationMessage = fmt.Sprintf("Power connected. Switching to %s profile. Battery: %d%% (%s)", newProfile, batteryCapacity, batteryStatus)
	} else { // On battery
		if batteryCapacity <= lowThreshold {
			newProfile = onBatteryLowProfile
			notificationMessage = fmt.Sprintf("Battery low (%d%%). Switching to %s profile.", batteryCapacity, newProfile)
		} else {
			newProfile = onBatteryBalancedProfile
			notificationMessage = fmt.Sprintf("Running on battery (%d%%). Switching to %s profile.", batteryCapacity, newProfile)
		}
	}

	if newProfile != currentProfile {
		log.Printf("Calculated new profile: %s. Attempting to set.", newProfile)
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
