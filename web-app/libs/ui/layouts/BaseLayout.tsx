import React from 'react';
import { cn } from '../utils';

export interface BaseLayoutProps {
  className?: string;
  children?: React.ReactNode;
}

export interface BaseLayoutMenuProps {
  className?: string;
  children?: React.ReactNode;
}

export interface BaseLayoutContentProps {
  className?: string;
  children?: React.ReactNode;
}

const BaseLayoutRoot: React.FC<BaseLayoutProps> = ({ children, className }) => {
  const hasMenu = React.Children.toArray(children).some(
    (child) => React.isValidElement(child) && child.type === BaseLayout.Menu
  );

  return (
    <div
      className={cn(
        'w-screen h-screen',
        "grid gap-0 [grid-template-areas:'content']",
        hasMenu &&
          "grid-cols-[auto_1fr] md:grid-cols-[120px_1fr] [grid-template-areas:'menu_content']",
        className
      )}
    >
      {children}
    </div>
  );
};

BaseLayoutRoot.displayName = 'BaseLayout';

const Menu: React.FC<BaseLayoutMenuProps> = ({ children, className }) => (
  <div
    className={cn('bg-background-text fixed h-full hidden md:grid md:[grid-area:menu]', className)}
  >
    {children}
  </div>
);
Menu.displayName = 'BaseLayout.Menu';

const Content: React.FC<BaseLayoutContentProps> = ({ children, className }) => (
  <div className={cn('bg-background-page [grid-area:content]', className)}>{children}</div>
);
Content.displayName = 'BaseLayout.Content';

export const BaseLayout = Object.assign(BaseLayoutRoot, {
  Menu,
  Content
});
