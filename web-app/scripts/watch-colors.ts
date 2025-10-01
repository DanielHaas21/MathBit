/* This script was copilot generated for my colors setup */

/* eslint-disable no-console */
import fs from 'fs';
import path from 'path';
import { generateColorsCSS } from './generate-colors';

const colorsPath = path.join(__dirname, '../libs/ui/colors/index.ts');

console.log('🔍 Watching for changes to colors file...');
console.log(`📁 Watching: ${path.relative(process.cwd(), colorsPath)}`);

// Initial generation
generateColorsCSS();

// Watch for changes
fs.watchFile(colorsPath, () => {
  console.log(`\n🔄 Colors file changed, regenerating CSS...`);
  generateColorsCSS();
});

console.log('\n👁️ Watcher is running. Press Ctrl+C to stop.');
