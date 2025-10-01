'use client';
import * as React from 'react';
import { cva, type VariantProps } from 'class-variance-authority';
import { cn } from '../utils';
import { Transition } from '@headlessui/react';
import { TooltipPosition } from '../types/TooltipPosition';

type BaseTooltipAtrributes = Pick<React.HTMLAttributes<HTMLDivElement>, 'className' | 'id'>;

const TooltipVariants = cva(
  `absolute !z-[1111] whitespace-normal rounded-md bg-black-950 p-2 text-xs text-text-white shadow `,
  {
    variants: {
      size: {
        sm: 'w-[100px]',
        md: 'w-[200px]',
        lg: 'w-[300px]',
        xl: 'w-[350px]'
      }
    },
    defaultVariants: {
      size: 'md'
    }
  }
);

type TooltipVariantProps = VariantProps<typeof TooltipVariants>;

export interface TooltipProps extends BaseTooltipAtrributes, TooltipVariantProps {
  label?: string;
  text?: string;
  position: TooltipPosition;
  children: React.ReactNode;
  trigger?: 'hover' | 'click';
}

export const Tooltip = React.forwardRef<HTMLDivElement, TooltipProps>(
  ({ label, text, position, trigger = 'hover', className, size, children, ...props }, ref) => {
    const [show, setShow] = React.useState<boolean>(false);

    const handleToggle = () => setShow((prev) => !prev);
    const handleShow = () => setShow(true);
    const handleHide = () => setShow(false);

    const getPositionClasses = (pos: TooltipPosition) => {
      switch (pos) {
        case 'top':
        case 'top center':
          return 'bottom-full left-1/2 -translate-x-1/2 mb-2';
        case 'top left':
          return 'bottom-full left-0 mb-2';
        case 'top right':
          return 'bottom-full right-0 mb-2';
        case 'bottom':
          return 'top-full left-1/2 -translate-x-1/2 mt-2';
        case 'left':
          return 'right-full top-1/2 -translate-y-1/2 mr-2';
        case 'right':
          return 'left-full top-1/2 -translate-y-1/2 ml-2';
        default:
          return 'bottom-full left-1/2 -translate-x-1/2 mb-2';
      }
    };

    const getArrowPositionClasses = (pos: TooltipPosition) => {
      switch (pos) {
        case 'top':
        case 'top center':
          return 'top-full left-1/2 -translate-x-1/2 mt-[-5px]';
        case 'top left':
          return 'top-full left-4 mt-[-5px]';
        case 'top right':
          return 'top-full right-4 mt-[-5px]';
        case 'bottom':
          return 'bottom-full left-1/2 -translate-x-1/2 mb-[-5px]';
        case 'left':
          return 'left-full top-1/2 -translate-y-1/2 ml-[-5px]';
        case 'right':
          return 'right-full top-1/2 -translate-y-1/2 mr-[-5px]';
        default:
          return 'top-full left-1/2 -translate-x-1/2 mt-[-5px]';
      }
    };

    return (
      <div
        ref={ref}
        className={cn('relative inline-block', className)}
        onMouseEnter={trigger === 'hover' ? handleShow : undefined}
        onMouseLeave={trigger === 'hover' ? handleHide : undefined}
        onFocus={trigger === 'hover' ? handleShow : undefined}
        onBlur={trigger === 'hover' ? handleHide : undefined}
        onClick={trigger === 'click' ? handleToggle : undefined}
        tabIndex={0}
        {...props}
      >
        {children}

        <Transition
          show={show}
          enter="transition-opacity duration-300"
          enterFrom="opacity-0"
          enterTo="opacity-100"
          leave="transition-opacity duration-300"
          leaveFrom="opacity-100"
          leaveTo="opacity-0"
        >
          <div className={cn(TooltipVariants({ size }), getPositionClasses(position))}>
            {label && <h2 className="font-semibold">{label}</h2>}
            {text && <div className="mt-1 text-[11px] text-text-white">{text}</div>}
            <div
              className={cn(
                'absolute w-2 h-2 bg-black-950 rotate-45',
                getArrowPositionClasses(position)
              )}
            />
          </div>
        </Transition>
      </div>
    );
  }
);
Tooltip.displayName = 'Tooltip';
