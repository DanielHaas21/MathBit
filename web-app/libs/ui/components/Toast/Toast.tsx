'use client';
import * as React from 'react';
import { cva, type VariantProps } from 'class-variance-authority';
import { cn } from '../../utils';
import { IconName } from '../../icons/names';
import { Icon } from '../Icon';
import { motion, AnimatePresence } from 'framer-motion';

const ToastWrapperVariants = cva(``, {
  variants: {
    size: {
      sm: 'w-[300px]',
      md: 'w-[400px]',
      lg: 'w-[500px]',
      full: 'w-full',
    },
  },
  defaultVariants: {
    size: 'md',
  },
});

type ToastWrapperVariantProps = VariantProps<typeof ToastWrapperVariants>;

export interface ToastWrapperProps extends ToastWrapperVariantProps {
  children: React.ReactNode | React.ReactNode[];
  removeTimer?: number | false;
  className?: string;
}

export const ToastWrapper = React.forwardRef<HTMLDivElement, ToastWrapperProps>(
  ({ children, className, size, removeTimer = 10000 }, ref) => {
    const [toasts, setToasts] = React.useState(React.Children.toArray(children));

    const handleRemove = (index: number) => {
      setToasts((prev) => prev.filter((_, i) => i !== index));
    };

    React.useEffect(() => {
      setToasts(React.Children.toArray(children));
    }, [children]);

    React.useEffect(() => {
      if (!removeTimer || toasts.length === 0) return;

      const timer = setTimeout(() => {
        setToasts((prev) => prev.slice(1));
      }, removeTimer);

      return () => clearTimeout(timer);
    }, [removeTimer, toasts.length]);

    return (
      <div
        ref={ref}
        className={cn(className, ToastWrapperVariants({ size }), 'flex flex-col gap-2')}
      >
        <AnimatePresence>
          {toasts.map((child, index) => {
            if (!React.isValidElement(child) || child.type !== ToastItem) {
              throw new Error('<Toast> only accepts <Toast.Item> as children.');
            }

            // Clone child with extra prop: onRemove
            return React.cloneElement(child as React.ReactElement<ToastItemProps>, {
              key: child.key || index,
              onRemove: () => handleRemove(index),
            });
          })}
        </AnimatePresence>
      </div>
    );
  }
);

const ToastItemVariants = cva(
  `flex flex-row items-center justify-between w-full p-[14px] shadow-lg rounded-lg relative !overflow-hidden`,
  {
    variants: {
      variant: {
        default: '',
        success: '',
        error: '',
        warning: '',
      },
      tint: {
        true: '',
        false: 'bg-white-50',
      },
    },
    compoundVariants: [
      {
        variant: 'default',
        tint: false,
        className: 'bg-white-50',
      },
      {
        variant: 'success',
        tint: false,
        className: 'bg-white-50',
      },
      {
        variant: 'success',
        tint: true,
        className: 'bg-success-bg text-success-text',
      },
      {
        variant: 'warning',
        tint: false,
        className: 'bg-white-50 ',
      },
      {
        variant: 'warning',
        tint: true,
        className: 'bg-warn-bg text-warn-text',
      },
      {
        variant: 'error',
        tint: false,
        className: 'bg-white-50',
      },
      {
        variant: 'error',
        tint: true,
        className: 'bg-error-bg  text-error-text',
      },
    ],
    defaultVariants: {
      variant: 'default',
      tint: false,
    },
  }
);

type ToastItemVariantProps = VariantProps<typeof ToastItemVariants>;

export interface ToastItemProps extends ToastItemVariantProps {
  icon?: IconName;
  children: React.ReactNode;
  description?: React.ReactNode;
  actions?: React.ReactNode;
  progressBar?: number;
  onRemove?: () => void;
}

export const ToastItem = React.forwardRef<HTMLDivElement, ToastItemProps>(
  (
    { children, variant = 'default', tint, icon, description, actions, progressBar, onRemove },
    ref
  ) => {
    const IconVariantColor = {
      default: 'text-text-black',
      success: 'text-success-icon',
      error: 'text-error-icon',
      warning: 'text-warn-icon',
    }[variant!];

    const BarVariantColor = {
      default: 'bg-text-black',
      success: 'bg-success-icon',
      error: 'bg-error-icon',
      warning: 'bg-warn-icon',
    }[variant!];

    return (
      <motion.div
        layout
        initial={{ opacity: 0, y: 40 }}
        animate={{ opacity: 1, y: 0 }}
        exit={{ opacity: 0, y: 40 }}
        role="alert"
        transition={{ duration: 0.4, ease: [0, 0.8, 0.4, 1] }}
        className={cn(ToastItemVariants({ variant, tint }))}
        ref={ref}
      >
        {icon && <Icon name={icon} className={cn(IconVariantColor, 'mr-3')} />}
        <div className="flex-1 overflow-hidden">
          <div className="font-medium">{children}</div>
          {description && <div className="text-sm text-black-800 mt-1">{description}</div>}
          {actions && <div className="mt-2 flex items-center space-x-2">{actions}</div>}
        </div>
        <Icon
          name="xmark"
          onClick={onRemove}
          className="cursor-pointer transition !text-text-black hover:!text-text-grey"
        />
        {typeof progressBar === 'number' && (
          <div
            className={cn(
              BarVariantColor,
              'absolute bottom-0 left-0 h-1  rounded-full transition-all duration-300'
            )}
            style={{ width: progressBar > 100 ? 416 : 416 * (progressBar / 100) }}
          />
        )}
      </motion.div>
    );
  }
);

const Toast = Object.assign(ToastWrapper, { Item: ToastItem });

export { Toast };

Toast.displayName = 'Toast';
ToastWrapper.displayName = 'ToastWrapper';
ToastItem.displayName = 'Toast.Item';
