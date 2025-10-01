import { ReactElement } from 'react';

export interface BreadcrumbItem {
  pageTitle?: ReactElement | string;
  locKey?: string | null;
  pageRoute: string;
}
