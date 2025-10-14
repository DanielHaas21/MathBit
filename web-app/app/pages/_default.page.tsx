import { Outlet } from 'react-router-dom'
export default function Layout({ children }: { children: React.ReactNode }) {
  return (
    <html>
      <head>
        <meta charSet="utf-8" />
      </head>
      <body>
        <header>My App</header>
        <main>{children || <Outlet />}</main>
      </body>
    </html>
  )
}