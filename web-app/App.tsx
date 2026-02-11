import React from 'react';
import { createRoot } from 'react-dom/client';
import { RouterProvider } from 'react-router-dom';
import { router } from './app/router';
import { I18nextProvider } from 'react-i18next';
import { initI18n } from './i18n/i18n';
import './index.css';
import { UiProvider } from './libs/ui/provider';
import { Provider } from 'react-redux';
import { store } from './store/store';

(async () => {
  const lang = navigator.language === 'cs-CZ' ? 'cs' : 'en';

  const i18n = await initI18n(lang, false);
  createRoot(document.getElementById('root')!).render(
    <Provider store={store}>
      <I18nextProvider i18n={i18n}>
        <UiProvider i18nValue={i18n}>
          <RouterProvider router={router} />
        </UiProvider>
      </I18nextProvider>
    </Provider>
  );
})();
