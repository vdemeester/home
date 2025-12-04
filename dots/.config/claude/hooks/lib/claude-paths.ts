#!/usr/bin/env bun

import { homedir } from 'os';
import { resolve, join } from 'path';
import { existsSync } from 'fs';

/**
 * Claude Code infrastructure paths
 *
 * Defaults to ~/.claude but can be overridden with CLAUDE_DIR env var
 */
export const CLAUDE_DIR = process.env.CLAUDE_DIR
  ? resolve(process.env.CLAUDE_DIR)
  : resolve(homedir(), '.claude');

export const HOOKS_DIR = join(CLAUDE_DIR, 'hooks');
export const SKILLS_DIR = join(CLAUDE_DIR, 'skills');
export const AGENTS_DIR = join(CLAUDE_DIR, 'agents');
export const HISTORY_DIR = join(CLAUDE_DIR, 'history');

/**
 * Validate Claude Code directory structure
 * Called automatically on import
 */
function validateClaudeStructure(): void {
  if (!existsSync(CLAUDE_DIR)) {
    console.error(`❌ CLAUDE_DIR does not exist: ${CLAUDE_DIR}`);
    console.error(`   Expected ~/.claude or set CLAUDE_DIR environment variable`);
    process.exit(1);
  }

  if (!existsSync(HOOKS_DIR)) {
    console.error(`⚠️  Claude hooks directory not found: ${HOOKS_DIR}`);
    console.error(`   This may be expected if hooks haven't been set up yet`);
    // Don't exit - this is okay for initial setup
  }
}

// Validate on import
validateClaudeStructure();

/**
 * Get a history file path with proper year-month organization
 * @param subdir - History subdirectory (sessions, learnings, research, etc.)
 * @param filename - File name
 * @returns Full path to history file
 */
export function getHistoryFilePath(subdir: string, filename: string): string {
  const now = new Date();
  const year = now.getFullYear();
  const month = String(now.getMonth() + 1).padStart(2, '0');

  return join(HISTORY_DIR, subdir, `${year}-${month}`, filename);
}

/**
 * Get current timestamp in YYYY-MM-DD-HHMMSS format
 */
export function getTimestamp(): string {
  const now = new Date();
  const year = now.getFullYear();
  const month = String(now.getMonth() + 1).padStart(2, '0');
  const day = String(now.getDate()).padStart(2, '0');
  const hours = String(now.getHours()).padStart(2, '0');
  const minutes = String(now.getMinutes()).padStart(2, '0');
  const seconds = String(now.getSeconds()).padStart(2, '0');

  return `${year}-${month}-${day}-${hours}${minutes}${seconds}`;
}
