import { test, expect } from '@playwright/test';

test('Opens app, logs in, goes to editor, types math expression, and submits, saves it, goes back to browser and searches for it, deletes', async ({
  page,
}) => {
  await page.goto('/');

  // go to editor
  await page.getByRole('button', { name: 'Login' }).click();

  // fill in login form
  await page.getByPlaceholder('Enter your email').fill('Test@email.com');
  await page.getByPlaceholder('Enter your password').fill('Password');
  await page.getByRole('button', { name: 'Login' }).click();

  await page.getByRole('button', { name: 'Go To Editor' }).click();

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

  // save the expression
  await page.getByRole('button', { name: 'Save as new Problem' }).click();
  // fill in problem name
  await page.getByPlaceholder('Enter problem name').fill('Test Problem');
  await page.getByRole('button', { name: 'Save', exact: true }).click();

  // go back to browser, manually, because a headless-ui modal is blocking the way and I don't want to deal with it in the test 
  await page.goto('/browser');

  // search for the problem
  await page.getByPlaceholder('Search problems...').fill('Test Problem');

  // check if it appears in the search results
  await expect(page.getByTestId(`head-label-Test Problem`)).toBeVisible();

  // delete the problem
  await page.getByRole('button', { name: 'Delete' }).click();

  await page.getByRole('button', { name: 'Yes' }).click();

  // check if it has been deleted
  await expect(page.getByTestId(`head-label-Test Problem`)).not.toBeVisible();
});
