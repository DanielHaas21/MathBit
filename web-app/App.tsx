import React from 'react';
import { createRoot } from 'react-dom/client';
import { RouterProvider } from 'react-router-dom';
import { router } from './app/router';
import { I18nextProvider } from 'react-i18next';
import { initI18n } from './i18n/i18n';
import './token'; // interceptor

(async () => {
  const i18n = await initI18n('en', false);

  createRoot(document.getElementById('root')!).render(
    <I18nextProvider i18n={i18n}>
      <RouterProvider router={router} />
    </I18nextProvider>
  );
})();
