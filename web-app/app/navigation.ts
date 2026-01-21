'use client';

export interface Route {
  href: string;
  subpages?: string[];
}

export const Routes: Route[] = [
  { href: '/' },
  {
    href: '/browser',
    subpages: ['/browser', '/browser/editor', '/browser/editor/[id]'],
  },
  { href: '/settings' },
  { href: '/signup' },
  { href: '/login' },
];

/**
 * Returns route by href (or subpage match)
 */
export const getRoute = (href: string) =>
  Routes.find((r) => r.href === href || r.subpages?.some((sub) => sub.startsWith(href)));

/**
 *
 * Replaces placeholders with params and returns a href for that
 *
 * There are no explicit return types, it has to be casted manually
 * but isnt expected since if you want a route use the get route methods
 * @param pattern Link with placeholders
 * @param params Key-value pair where the key has the same name as the placeholder
 * @returns Reconstructed href
 */
export const buildHref = (pattern: string, params?: Record<string, string | number>) => {
  if (!params) return pattern;

  let finalHref = pattern;
  for (const [key, value] of Object.entries(params)) {
    finalHref = finalHref.replace(`[${key}]`, encodeURIComponent(String(value)));
  }
  return finalHref;
};

/**
 * Determines if the route is active eg. If the user is on that given page or a subpage of a given route
 * @param currentPath
 * @param route
 * @param placeholder
 * @returns bool
 */
export const isActiveRoute = (currentPath: string, route: Route, placeholder = 'id') =>
  currentPath === route.href ||
  route.subpages?.some((sub) => {
    const pattern = new RegExp(`^${sub.replace(`[${placeholder}]`, '[^/]+')}$`);
    return pattern.test(currentPath);
  });

// Url is for next links and string is for anything else
export function getHome(): string {
  return getRoute('/')?.href as string;
}

export function getBrowser(): string {
  return getRoute('/browser')?.href as string;
}

export function getEditor(): string {
  return getRoute('/browser/editor')?.href as string;
}
