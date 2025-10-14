import i18next from 'i18next'
import Backend from 'i18next-fs-backend'
import { initReactI18next } from 'react-i18next'
import { resolve } from 'path'

export async function initI18n(lng = 'en') {
  await i18next
    .use(Backend)
    .use(initReactI18next)
    .init({
      lng,
      fallbackLng: 'en',
      supportedLngs: ['en', 'cs'],
      ns: ['common'],
      defaultNS: 'common',
      backend: {
        loadPath: resolve('./i18n/{{lng}}/{{ns}}.json'),
      },
      react: { useSuspense: false },
    })

  return i18next
}
