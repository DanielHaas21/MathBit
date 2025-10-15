import i18next from 'i18next';
import { initReactI18next } from 'react-i18next';

export async function initI18n(lng = 'en', isServer = false) {
  const options: any = {
    lng,
    fallbackLng: 'en',
    supportedLngs: ['en', 'cs'],
    ns: ['common'],
    defaultNS: 'common',
    react: { useSuspense: false },
  };

  if (isServer) {
    // Node backend
    const Backend = await import('i18next-fs-backend').then((m) => m.default);
    const { resolve } = await import('path');
    options.backend = { loadPath: resolve('./i18n/{{lng}}/{{ns}}.json') };
    i18next.use(Backend);
  } else {
    // Browser backend
    const Backend = await import('i18next-http-backend').then((m) => m.default);
    options.backend = { loadPath: '/i18n/{{lng}}/{{ns}}.json' };
    i18next.use(Backend);
  }

  i18next.use(initReactI18next);

  await i18next.init(options);

  return i18next;
}
