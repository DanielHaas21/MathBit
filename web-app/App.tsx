import React, { Suspense } from 'react';
import { createRoot } from 'react-dom/client';
import { RouterProvider } from 'react-router-dom';
import { router } from './app/router';
import i18n from './public/i18n/i18n';
import './index.css';
import { UiProvider } from './libs/ui/provider';
import { Provider } from 'react-redux';
import { store } from './store/store';
import { Loader } from './libs/ui/components';

(async () => {
  createRoot(document.getElementById('root')!).render(
    <Provider store={store}>
      <Suspense fallback={<Loader />}>
        <UiProvider i18nValue={i18n}>
          <RouterProvider router={router} />
        </UiProvider>
      </Suspense>
    </Provider>
  );
})();
