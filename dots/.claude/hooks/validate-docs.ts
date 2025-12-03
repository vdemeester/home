#!/usr/bin/env bun

/**
 * Documentation Link Validator
 *
 * Validates that all internal markdown links point to existing files.
 * Useful as a pre-commit hook to prevent broken documentation.
 *
 * Usage:
 *   bun run ~/.claude/hooks/validate-docs.ts
 *
 * Exit codes:
 *   0 - All links valid
 *   1 - Broken links found
 */

import { readFileSync, existsSync } from 'fs';
import { join, dirname, resolve } from 'path';
import { Glob } from 'bun';

// ANSI color codes
const colors = {
  reset: '\x1b[0m',
  red: '\x1b[31m',
  green: '\x1b[32m',
  yellow: '\x1b[33m',
  cyan: '\x1b[36m',
};

interface BrokenLink {
  file: string;
  link: string;
  target: string;
  line: number;
}

/**
 * Extract markdown links from content
 */
function extractLinks(content: string): { link: string; line: number }[] {
  const links: { link: string; line: number }[] = [];
  const lines = content.split('\n');

  // Match [text](path) style links
  const linkRegex = /\[([^\]]+)\]\(([^)]+)\)/g;

  lines.forEach((line, index) => {
    let match;
    while ((match = linkRegex.exec(line)) !== null) {
      const link = match[2];

      // Skip external URLs, anchors, and mailto links
      if (link.startsWith('http://') ||
          link.startsWith('https://') ||
          link.startsWith('#') ||
          link.startsWith('mailto:')) {
        continue;
      }

      links.push({ link, line: index + 1 });
    }
  });

  return links;
}

/**
 * Resolve a link path relative to the file
 */
function resolveLink(fromFile: string, linkPath: string, baseDir: string): string {
  // Remove anchor if present
  const pathWithoutAnchor = linkPath.split('#')[0];

  // If it starts with ~, expand to home directory
  if (pathWithoutAnchor.startsWith('~/')) {
    return resolve(process.env.HOME || '', pathWithoutAnchor.substring(2));
  }

  // If it's absolute, use as-is
  if (pathWithoutAnchor.startsWith('/')) {
    return pathWithoutAnchor;
  }

  // Otherwise, resolve relative to the file's directory
  const fileDir = dirname(fromFile);
  return resolve(fileDir, pathWithoutAnchor);
}

/**
 * Validate markdown files in a directory
 */
function validateDocs(baseDir: string): BrokenLink[] {
  const brokenLinks: BrokenLink[] = [];

  // Find all markdown files
  const glob = new Glob('**/*.md');

  for (const file of glob.scanSync({ cwd: baseDir, absolute: false })) {
    const filePath = join(baseDir, file);

    // Skip node_modules and hidden directories
    if (filePath.includes('node_modules') || filePath.includes('/.')) {
      continue;
    }

    try {
      const content = readFileSync(filePath, 'utf-8');
      const links = extractLinks(content);

      for (const { link, line } of links) {
        const targetPath = resolveLink(filePath, link, baseDir);

        // Check if target exists
        if (!existsSync(targetPath)) {
          brokenLinks.push({
            file: file,
            link: link,
            target: targetPath,
            line: line,
          });
        }
      }
    } catch (error) {
      console.error(`${colors.yellow}Warning: Could not read ${file}${colors.reset}`);
    }
  }

  return brokenLinks;
}

function main(): number {
  const baseDir = process.cwd();

  console.log(`\n${colors.cyan}🔍 Documentation Link Validator${colors.reset}`);
  console.log(`${colors.cyan}   Base directory: ${baseDir}${colors.reset}\n`);

  const brokenLinks = validateDocs(baseDir);

  if (brokenLinks.length > 0) {
    console.log(`\n${colors.red}❌ Found ${brokenLinks.length} broken link(s):${colors.reset}\n`);

    for (const { file, link, line } of brokenLinks) {
      console.log(`  ${colors.yellow}${file}:${line}${colors.reset}`);
      console.log(`    → ${colors.red}${link}${colors.reset} (not found)\n`);
    }

    console.log(`\n${colors.red}Documentation validation failed. Please fix the broken links.${colors.reset}\n`);
    return 1;
  }

  console.log(`${colors.green}✅ All documentation links are valid${colors.reset}\n`);
  return 0;
}

process.exit(main());
