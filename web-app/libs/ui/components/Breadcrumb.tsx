'use client';
import * as React from 'react';
import { BreadcrumbItem } from '../types/BreadcrumbItem';
import { Icon } from './Icon';
import { cn } from '../utils';
import { useTranslation } from '../provider';
import { AnimatePresence, motion } from 'framer-motion';
import { Link } from 'react-router-dom';

export interface BreadcrumbProps {
  className?: string;
  route: BreadcrumbItem[];
}

export const Breadcrumb = React.forwardRef<HTMLDivElement, BreadcrumbProps>(
  ({ className, route, ...props }, ref) => {
    const t = useTranslation("");
    return (
      <div
        ref={ref}
        {...props}
        className={cn(className, 'inline-flex items-center flex-nowrap transition-all')}
      >
        {route.map((r, index) =>
          index !== route.length - 1 ? (
            <React.Fragment key={`${index}-frag`}>
              <Link
                key={`a-${index}`}
                className={cn('text-text-black', 'hover:text-link-hover')}
                to={r.pageRoute}
              >
                {r.locKey ? t(`breadcrumb.${r.locKey}`) : r.pageTitle}
              </Link>
              <Icon key={`i-${index}`} name="chevron-down" className="px-1" rotate={270}></Icon>
            </React.Fragment>
          ) : (
            <AnimatePresence key={`an-${index}`}>
              <motion.div
                key={`m-${index}`}
                initial={{ opacity: 0, y: 10 }}
                animate={{ opacity: 1, y: 0 }}
                exit={{ opacity: 0, y: 10 }}
                transition={{ duration: 0.4, ease: [0, 0.8, 0.4, 1] }}
              >
                <Link
                  key={`a-${index}`}
                  className={cn(
                    route.length > 1 ? 'text-text-grey' : 'text-text-black',
                    'hover:text-link-hover'
                  )}
                  to={r.pageRoute}
                >
                  {r.locKey ? t(`breadcrumb.${r.locKey}`) : r.pageTitle}
                </Link>
              </motion.div>
            </AnimatePresence>
          )
        )}
      </div>
    );
  }
);

Breadcrumb.displayName = 'Breadcrumb';
