/* This script was by copilot generated for my colors setup */

/* eslint-disable no-console */
import fs from 'fs';
import path from 'path';

/**
 * Converts hex color to RGB values string (without rgb() wrapper)
 * Example: '#FF6901' -> '255 105 1'
 */
function hexToRgb(hex: string): string {
  const result = /^#?([a-f\d]{2})([a-f\d]{2})([a-f\d]{2})$/i.exec(hex);
  if (!result) {
    throw new Error(`Invalid hex color: ${hex}`);
  }
  const r = parseInt(result[1]!, 16);
  const g = parseInt(result[2]!, 16);
  const b = parseInt(result[3]!, 16);
  return `${r} ${g} ${b}`;
}

/**
 * Converts a JavaScript object path to CSS custom property name for base variables
 * Example: 'brandBlue.500' -> '--brand-blue-500'
 */
function convertToBaseVar(key: string, parent = '') {
  const fullKey = parent ? `${parent}-${key}` : key;
  return `--${fullKey.replace(/([A-Z])/g, '-$1').toLowerCase()}`;
}

/**
 * Converts a JavaScript object path to CSS custom property name for theme variables
 * Example: 'brandBlue.500' -> '--color-brand-blue-500'
 */
function convertToThemeVar(key: string, parent = '') {
  const fullKey = parent ? `${parent}-${key}` : key;
  return `--color-${fullKey.replace(/([A-Z])/g, '-$1').toLowerCase()}`;
}

/**
 * Recursively converts color object to base CSS custom properties (RGB values)
 */
function convertColorsToBaseCSS(colors: Record<string, string>, parentKey = '') {
  let cssContent = '';

  for (const [key, value] of Object.entries(colors)) {
    if (typeof value === 'object' && value !== null && !Array.isArray(value)) {
      // Recursive case for nested objects
      cssContent += convertColorsToBaseCSS(value, parentKey ? `${parentKey}-${key}` : key);
    } else if (typeof value === 'string') {
      // Skip rgba values for base layer
      if (value.startsWith('rgba(')) {
        continue;
      }
      // Base case for color values - convert hex to RGB
      const baseVar = convertToBaseVar(key, parentKey);
      const rgbValue = hexToRgb(value);
      cssContent += `    ${baseVar}: ${rgbValue};\n`;
    }
  }

  return cssContent;
}

/**
 * Recursively converts color object to theme CSS custom properties (rgb() references)
 */
function convertColorsToThemeCSS(colors: Record<string, string>, parentKey = '') {
  let cssContent = '';

  for (const [key, value] of Object.entries(colors)) {
    if (typeof value === 'object' && value !== null && !Array.isArray(value)) {
      // Recursive case for nested objects
      cssContent += convertColorsToThemeCSS(value, parentKey ? `${parentKey}-${key}` : key);
    } else if (typeof value === 'string') {
      const themeVar = convertToThemeVar(key, parentKey);

      if (value.startsWith('rgba(')) {
        // For rgba values, keep them as-is
        cssContent += `  ${themeVar}: ${value};\n`;
      } else {
        // For hex values, reference the base variable
        const baseVar = convertToBaseVar(key, parentKey);
        cssContent += `  ${themeVar}: rgb(var(${baseVar}));\n`;
      }
    }
  }

  return cssContent;
}

/**
 * Processes overlay colors with rgba values for theme section
 */
function processOverlaysTheme(overlays: Record<string, string>) {
  let cssContent = '';
  for (const [key, value] of Object.entries(overlays)) {
    cssContent += `  --color-overlays-${key}: ${value};\n`;
  }
  return cssContent;
}

export default async function generateColorsCSS() {
  try {
    const colorsPath = path.join(__dirname, '../libs/ui/colors/index.ts');
    const outputPath = path.join(__dirname, '../libs/ui/assets/styles/colors.css');

    // Read the TypeScript colors file
    const colorsContent = fs.readFileSync(colorsPath, 'utf8');

    // Create a temporary module to evaluate the colors
    const tempFilePath = path.join(__dirname, '../temp-colors.js');

    // Convert TypeScript to JavaScript by removing types and export syntax
    const jsContent = colorsContent
      .replace(/const colors:/g, 'const colors =')
      .replace(/export default colors;/g, 'module.exports = colors;');

    fs.writeFileSync(tempFilePath, jsContent);

    // Import the colors using dynamic import
    const colorsModule = await import(tempFilePath);
    const colors = colorsModule.default || colorsModule;

    // Clean up temp file
    fs.unlinkSync(tempFilePath);

    // Generate base layer CSS content (RGB values)
    let baseCssContent = '';
    const { overlays, ...otherColors } = colors;
    baseCssContent += convertColorsToBaseCSS(otherColors);

    // Generate theme layer CSS content (rgb() references)
    let themeCssContent = '';
    themeCssContent += convertColorsToThemeCSS(otherColors);

    // Process overlays separately for theme section (they stay as rgba)
    if (overlays) {
      themeCssContent += '\n  /* Overlay colors with alpha */\n';
      themeCssContent += processOverlaysTheme(overlays);
    }

    // Combine everything into final CSS
    const finalCSS = `/* Auto-generated from src/colors/index.ts - Do not edit manually */

@theme {
${baseCssContent}${themeCssContent}}`;

    // Ensure output directory exists
    const outputDir = path.dirname(outputPath);
    if (!fs.existsSync(outputDir)) {
      fs.mkdirSync(outputDir, { recursive: true });
    }

    // Write the CSS file
    fs.writeFileSync(outputPath, finalCSS);

    console.log(`✅ Generated colors.css from TypeScript colors definition`);
    console.log(`📁 Output: ${path.relative(process.cwd(), outputPath)}`);
  } catch (error: unknown) {
    if (error instanceof Error) {
      console.error('❌ Error generating colors.css:', error.message);
    } else {
      console.error('❌ Error generating colors.css:', error);
    }
    process.exit(1);
  }
}

// Run if called directly
if (require.main === module) {
  generateColorsCSS();
}
