import { cn } from '../utils';
import { cva, type VariantProps } from 'class-variance-authority';
import { Menu, MenuButton, MenuItem, MenuItems, Transition } from '@headlessui/react';
import { IconName } from '../icons/names';
import { Divider } from './Divider';
import { Icon } from './Icon';
import React from 'react';
import { AnchorTo } from '../types/Anchor';

type BaseDropdownMenuAttributes = Pick<
  React.HTMLAttributes<HTMLDivElement>,
  'id' | 'className' | 'onChange'
>;

const DropdownMenuVariants = cva(
  `bg-white-50 border !border-white-950 rounded-lg shadow-lg absolute  min-h-[120px] z-50 mt-2 overflow-y-hidden`,
  {
    variants: {
      size: {
        sm: 'w-[200px]',
        md: 'w-[240px]',
        lg: 'w-[280px]'
      }
    },
    defaultVariants: {
      size: 'md'
    }
  }
);

type DropdownMenuVariantProps = VariantProps<typeof DropdownMenuVariants>;

/**
 * Base of the dropdown menu
 */
export interface DropdownMenuProps extends BaseDropdownMenuAttributes, DropdownMenuVariantProps {
  triggerButton: React.ReactNode;
  HeadItem?: React.ReactNode;
  children: React.ReactNode;
  className?: string;
  align?: AnchorTo;
}

export const DropdownMenu = React.forwardRef<HTMLDivElement, DropdownMenuProps>(
  ({ triggerButton, HeadItem, children, className, align = 'bottom', size }, ref) => {
    const HeadItemSize = {
      sm: 'w-[198px]',
      md: 'w-[238px]',
      lg: 'w-[278px]'
    }[size ? size : 'md'];

    React.Children.forEach(children, (child) => {
      if (!React.isValidElement(child) || child.type !== DropdownItem) {
        throw new Error('<Dropdown> only accepts <Dropdown.Item> as children.');
      }
    });

    return (
      <Menu ref={ref}>
        <MenuButton aria-haspopup="true" aria-expanded="false">
          {triggerButton}
        </MenuButton>
        <Transition
          enter="transition ease-out duration-200"
          enterFrom="opacity-0 translate-y-1"
          enterTo="opacity-100 translate-y-0"
          leave="transition ease-in duration-150"
          leaveFrom="opacity-100 translate-y-0"
          leaveTo="opacity-0 translate-y-1"
        >
          <MenuItems
            as="div"
            anchor={align}
            className={cn(className, DropdownMenuVariants({ size }))}
          >
            {HeadItem && (
              <MenuItem
                onClick={(e) => {
                  e.preventDefault();
                  e.stopPropagation();
                }}
                as="div"
                className="flex flex-col flex-nowrap justify-start items-center text-md transition rounded"
              >
                <div
                  className={cn(
                    'px-3 py-3 flex flex-row flex-nowrap justify-start items-center',
                    HeadItemSize
                  )}
                >
                  {HeadItem}
                </div>
                <Divider size="full" variant="light" thickness="sm"></Divider>
              </MenuItem>
            )}

            {children}
          </MenuItems>
        </Transition>
      </Menu>
    );
  }
);

DropdownMenu.displayName = 'DropdownMenu';

/**
 * Dropdown item
 */
type BaseDropdownItemAttributes = Pick<
  React.HTMLAttributes<HTMLDivElement>,
  'id' | 'className' | 'onChange' | 'onClick' | 'onFocus' | 'onMouseOver'
>;

export interface DropdownItemProps extends BaseDropdownItemAttributes {
  icon?: IconName;
  HasDivider?: boolean;
  hint?: string;
  disabled?: boolean;
  children: React.ReactNode;
  className?: string;
}

const DropdownItem = React.forwardRef<HTMLDivElement, DropdownItemProps>(
  ({ icon, HasDivider = false, hint, children, className, onClick, disabled, ...props }, ref) => {
    return (
      <>
        {HasDivider && <Divider size="full" variant="light" thickness="sm" />}
        <MenuItem
          as="div"
          ref={ref}
          disabled={disabled}
          onClick={(e) => {
            if (disabled) return;
            e.preventDefault();
            e.stopPropagation();
            onClick?.(e);
          }}
          aria-disabled={disabled}
          className={cn(
            'flex flex-row flex-nowrap justify-between items-center px-3 py-2 cursor-pointer text-sm rounded',
            disabled && 'opacity-50 cursor-not-allowed',
            !disabled && 'hover:bg-white-200 transition duration-300',
            className
          )}
          {...props}
        >
          <div className="flex items-center font-normal">
            {icon && <Icon name={icon} className="mr-3 mb-[2px]" />}
            {children}
          </div>
          {hint && (
            <div className="ml-3">
              <p className="text-xs text-text-grey font-light">{hint}</p>
            </div>
          )}
        </MenuItem>
      </>
    );
  }
);

DropdownItem.displayName = 'DropdownItem';

const Dropdown = Object.assign(DropdownMenu, {
  Item: DropdownItem
});

export { Dropdown };
