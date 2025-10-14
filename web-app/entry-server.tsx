import { renderToString } from 'react-dom/server'
import { I18nextProvider } from 'react-i18next'
import { initI18n } from './i18n/i18n'

function PageShell({
  children,
}: {
  children: React.ReactNode
}) {
  return (
    <html>
      <head>
        <meta charSet="utf-8" />
        <title>My App</title>
      </head>
      <body>
        <header>My SSR App 🧭</header>
        <main>{children}</main>
      </body>
    </html>
  )
}

export async function render(pageContext: any) {
  const i18n = await initI18n('en')
  const { Page, pageProps } = pageContext

  const pageHtml = renderToString(
    <I18nextProvider i18n={i18n}>
      <PageShell>
        <Page {...pageProps} />
      </PageShell>
    </I18nextProvider>
  )

  return {
    documentHtml: `<!DOCTYPE html>
      <html>
        <head><meta charset="utf-8"/></head>
        <body>
          <div id="root">${pageHtml}</div>
        </body>
      </html>`,
  }
}
