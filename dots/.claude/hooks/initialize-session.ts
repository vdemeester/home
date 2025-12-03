#!/usr/bin/env bun

/**
 * initialize-session.ts
 *
 * Session initialization hook that runs at the start of every Claude Code session.
 *
 * What it does:
 * - Checks if this is a subagent session (skips for subagents)
 * - Sets initial terminal tab title
 * - Logs session start
 *
 * Setup:
 * Add to SessionStart in settings.json:
 * "SessionStart": ["bun /home/vincent/.claude/hooks/initialize-session.ts"]
 */

import { existsSync, mkdirSync, appendFileSync } from 'fs';
import { join } from 'path';
import { tmpdir } from 'os';
import { CLAUDE_DIR, getHistoryFilePath, getTimestamp } from './lib/claude-paths';

// Debounce duration in milliseconds (prevents duplicate SessionStart events)
const DEBOUNCE_MS = 2000;
const LOCKFILE = join(tmpdir(), 'claude-session-start.lock');

/**
 * Check if we're within the debounce window to prevent duplicate notifications
 */
function shouldDebounce(): boolean {
  try {
    if (existsSync(LOCKFILE)) {
      const lockContent = Bun.file(LOCKFILE).text();
      const lockTime = parseInt(await lockContent, 10);
      const now = Date.now();

      if (now - lockTime < DEBOUNCE_MS) {
        // Within debounce window, skip this notification
        return true;
      }
    }

    // Update lockfile with current timestamp
    await Bun.write(LOCKFILE, Date.now().toString());
    return false;
  } catch (error) {
    // If any error, just proceed (don't break session start)
    try {
      await Bun.write(LOCKFILE, Date.now().toString());
    } catch {}
    return false;
  }
}

async function main() {
  try {
    // Check if this is a subagent session - if so, exit silently
    const claudeProjectDir = process.env.CLAUDE_PROJECT_DIR || '';
    const isSubagent = claudeProjectDir.includes('/.claude/agents/') ||
                      process.env.CLAUDE_AGENT_TYPE !== undefined;

    if (isSubagent) {
      // This is a subagent session - exit silently without notification
      console.error('🤖 Subagent session detected - skipping session initialization');
      process.exit(0);
    }

    // Check debounce to prevent duplicate notifications
    if (await shouldDebounce()) {
      console.error('🔇 Debouncing duplicate SessionStart event');
      process.exit(0);
    }

    // Set initial tab title
    const tabTitle = 'Claude Ready';
    process.stderr.write(`\x1b]0;${tabTitle}\x07`);
    process.stderr.write(`\x1b]2;${tabTitle}\x07`);
    process.stderr.write(`\x1b]30;${tabTitle}\x07`);
    console.error(`📍 Session initialized: "${tabTitle}"`);

    // Log session start to history (optional)
    const timestamp = getTimestamp();
    const logDir = join(CLAUDE_DIR, 'history', 'sessions', `${timestamp.substring(0, 7)}`);

    if (!existsSync(logDir)) {
      mkdirSync(logDir, { recursive: true });
    }

    const logEntry = `${new Date().toISOString()} - Session started\n`;
    const logFile = join(logDir, `${timestamp.substring(0, 10)}_session-log.txt`);

    try {
      appendFileSync(logFile, logEntry);
    } catch (error) {
      // Silent failure - don't break session start for logging issues
    }

    process.exit(0);
  } catch (error) {
    console.error('SessionStart hook error:', error);
    process.exit(1);
  }
}

main();
