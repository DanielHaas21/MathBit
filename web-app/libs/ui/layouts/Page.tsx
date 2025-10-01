import React from 'react';
import { cn } from '../utils';

export interface PageProps {
  className?: string;
  children?: React.ReactNode;
}

export interface PageHeaderProps {
  className?: string;
  children?: React.ReactNode;
}

export interface PageTitleProps {
  className?: string;
  children?: React.ReactNode;
}

export interface PageActionProps {
  className?: string;
  children?: React.ReactNode;
}

export interface PageContentProps {
  className?: string;
  children?: React.ReactNode;
}

const PageRoot: React.FC<PageProps> = ({ children, className }) => {
  return (
    <div
      style={{ margin: '7px' }}
      className={cn(
        'grid gap-0 grid-cols-[1fr_auto]',
        "[grid-template-areas:'header_header'_'title_title'_'action_action'_'content_content'] sm:![grid-template-areas:'header_header'_'title_action'_'content_content']",
        'bg-background-page p-5',
        className
      )}
    >
      {children}
    </div>
  );
};

PageRoot.displayName = 'Page';

const Header: React.FC<PageHeaderProps> = ({ children, className }) => (
  <div className={cn('text-xs [grid-area:header]', className)}>{children}</div>
);
Header.displayName = 'Page.Header';

const Title: React.FC<PageTitleProps> = ({ children, className }) => (
  <div className={cn('py-2 text-2xl [grid-area:title]', className)}>{children}</div>
);
Title.displayName = 'Page.Title';

const Action: React.FC<PageActionProps> = ({ children, className }) => (
  <div className={cn('py-2 [grid-area:action]', className)}>{children}</div>
);
Action.displayName = 'Page.Action';

const Content: React.FC<PageContentProps> = ({ children, className }) => (
  <div className={cn('py-2 [grid-area:content] ms-[-2%] md:!ms-0', className)}>{children}</div>
);
Content.displayName = 'Page.Content';

// --------------------
// Compose
// --------------------
export const Page = Object.assign(PageRoot, {
  Header,
  Title,
  Action,
  Content
});
