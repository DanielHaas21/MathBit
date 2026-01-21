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
  return (
    <div
      className={cn(
        'w-screen h-screen',
        'flex flex-col justify-start',

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
    className={cn('bg-white-50 fixed] min-h-[150px] w-full flex', className)}
  >
    {children}
  </div>
);
Menu.displayName = 'BaseLayout.Menu';

const Content: React.FC<BaseLayoutContentProps> = ({ children, className }) => (
  <div className={cn('w-full flex min-h-[150px]', className)}>{children}</div>
);
Content.displayName = 'BaseLayout.Content';

export const BaseLayout = Object.assign(BaseLayoutRoot, {
  Menu,
  Content,
});
