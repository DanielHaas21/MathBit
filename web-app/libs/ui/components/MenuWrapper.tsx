import { cva, type VariantProps } from 'class-variance-authority';
import * as React from 'react';
import { cn } from '../utils';
import { MenuWrapperItem } from '../types/MenuWrapperItem';

const MenuWrapperVariants = cva(`flex flex-nowrap bg-white-50`, {
  variants: {
    orientation: {
      horizontal: 'flex-row',
      vertical: 'flex-col'
    }
  },
  defaultVariants: {
    orientation: 'vertical'
  }
});

type MenuWrapperVariantProps = VariantProps<typeof MenuWrapperVariants>;

export interface MenuWrapperProps extends MenuWrapperVariantProps {
  className?: string;
  items: MenuWrapperItem[];
}

export const MenuWrapper = React.forwardRef<HTMLDivElement, MenuWrapperProps>(
  ({ className, items = [], orientation = 'vertical', ...props }, ref) => {
    return (
      <div {...props} className={cn(MenuWrapperVariants({ orientation }), className)} ref={ref}>
        {items.map((items, index) => (
          <div
            className={cn(
              items.bordered
                ? orientation === 'vertical'
                  ? 'border-t border-t-white-500'
                  : 'border-s border-s-white-500'
                : '',
              'pt-2 pb-2 ps-2 pr-2'
            )}
            key={index}
          >
            {items.item}
          </div>
        ))}
      </div>
    );
  }
);

MenuWrapper.displayName = 'MenuWrapper';
