import { test, expect } from '@playwright/test';

test('Opens app, goes to editor, types math expression, submits, checks result', async ({
  page,
}) => {
  await page.goto('/');

  // go to editor
  await page.getByRole('button', { name: 'Begin' }).click();

  // grab MathQuill textarea
  const mqContainer = page.locator('.react-math-keyboard-input');
  await mqContainer.click();

  // focus it
  await page.keyboard.type('2+2');

  // optional: wait for math DOM to update
  await expect(page.locator('.mq-root-block')).not.toHaveClass(/mq-empty/);

  // submit
  await page.getByRole('button', { name: 'Solve' }).click();

  // check if the result is correct
  await expect(page.locator('.MathJax mjx-math')).toHaveAttribute('data-latex', '4');
});
