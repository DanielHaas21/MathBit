import { cva, type VariantProps } from 'class-variance-authority';
import * as React from 'react';
import { cn } from '../utils';
import { Icon } from './Icon';
import { IconName } from '../icons/names';

type baseMenuButtonAttributes = Pick<
  React.HTMLAttributes<HTMLDivElement>,
  'className' | 'onClick' | 'onFocus'
>;

const MenuButtonVariants = cva(
  `
    hover:scale-[1.02]
    active:scale-[0.98]
    transition-all
    rounded-lg
    flex items-center flex-col flex-nowrap justify-center font-regular
    text-md
    cursor-pointer
    select-none
    `,
  {
    variants: {
      variant: {
        inactive: 'bg-transparent text-text-black hover:bg-brand-blue-50',
        active: 'bg-primary-base text-white-50'
      },
      orientation: {
        vertical:
          'min-w-[100px] min-h-[70px] rounded-lg flex items-center flex-col flex-nowrap justify-center pb-1 pt-2 p-1',
        horizontal:
          'min-w-[200px] min-h-[50px] rounded-lg flex items-center flex-row flex-nowrap justify-start text-left'
      }
    },
    defaultVariants: {
      variant: 'inactive',
      orientation: 'vertical'
    }
  }
);

type MenuButtonVariantProps = VariantProps<typeof MenuButtonVariants>;

export interface MenuButtonProps extends baseMenuButtonAttributes, MenuButtonVariantProps {
  className?: string;
  icon?: IconName | React.ReactNode;
  children: string;
}

export const MenuButton = React.forwardRef<HTMLDivElement, MenuButtonProps>(
  ({ className, children, variant, icon = 'house', orientation, onClick, ...props }, ref) => {
    const handleKeyDown = (e: React.KeyboardEvent<HTMLDivElement>) => {
      if (e.key === 'Enter' || e.key === ' ') {
        e.preventDefault();
        onClick?.(e as unknown as React.MouseEvent<HTMLDivElement>);
      }
    };

    return (
      <div
        ref={ref}
        role="button"
        tabIndex={0}
        {...props}
        onClick={onClick}
        onKeyDown={handleKeyDown}
        className={cn(MenuButtonVariants({ variant, orientation }), className)}
      >
        {typeof icon === 'string' ? (
          <Icon
            name={icon as IconName}
            className={orientation === 'horizontal' ? 'ms-4' : 'mt-1'}
          />
        ) : (
          icon
        )}

        <h2
          className={cn(
            orientation === 'horizontal' ? 'ms-4' : 'w-[90%] mt-1',
            'text-xs text-center'
          )}
        >
          {children}
        </h2>
      </div>
    );
  }
);

MenuButton.displayName = 'MenuButton';
