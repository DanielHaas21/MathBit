import * as React from 'react';
import { cva, type VariantProps } from 'class-variance-authority';
import { cn } from '../utils';
import { Icon } from './Icon';
type BaseCheckboxAttributes = Omit<React.ButtonHTMLAttributes<HTMLButtonElement>, 'onChange'>;

const checkboxVariants = cva(
  `relative flex items-center justify-center border transition-all   
  active:ring-2 active:ring-brand-blue-300 active:bg-white-50 duration-300 cursor-pointer  
  disabled:cursor-not-allowed !border-2
  `,
  {
    variants: {
      variant: {
        square: 'rounded-[6px]',
        radio: 'rounded-full',
        circle: 'rounded-full bg-primary-base'
      },
      size: {
        sm: 'w-[18px] h-[18px]',
        md: 'w-[20px] h-[20px]',
        lg: 'w-[22px] h-[22px]'
      },
      state: {
        unchecked: 'border-white-800 bg-white-50  hover:bg-brand-blue-50 hover:border-primary-base',
        checked: ''
      },
      disabledState: {
        true: 'hover:!bg-white-50 hover:!border-white-800 active:ring-0',
        false: ''
      }
    },
    compoundVariants: [
      {
        variant: 'square',
        state: 'checked',
        disabledState: false,
        className: 'border-primary-base bg-brand-blue-50 text-brand-blue-800'
      },
      {
        variant: 'square',
        state: 'checked',
        disabledState: true,
        className:
          'border-primary-base bg-white-50 text-brand-blue-800 hover:!border-primary-base !opacity-25'
      },
      {
        variant: 'square',
        state: 'unchecked',
        disabledState: true,
        className: 'border-white-800 bg-white-50 hover:bg-primary-base !opacity-25'
      },

      {
        variant: 'radio',
        state: 'checked',
        disabledState: false,
        className: 'border-primary-base bg-brand-blue-50 text-brand-blue-800'
      },
      {
        variant: 'radio',
        state: 'checked',
        disabledState: true,
        className:
          'border-primary-base bg-white-50 text-brand-blue-800 hover:!border-primary-base !opacity-25'
      },
      {
        variant: 'radio',
        state: 'unchecked',
        disabledState: true,
        className: 'border-white-800 bg-white-50 !opacity-25'
      },

      {
        variant: 'circle',
        state: 'checked',
        disabledState: false,
        className: 'border-primary-base bg-primary-base text-white'
      },
      {
        variant: 'circle',
        state: 'checked',
        disabledState: true,
        className:
          'border-primary-base bg-primary-base hover:!border-primary-base  hover:!bg-primary-base text-white !opacity-25'
      },
      {
        variant: 'circle',
        state: 'unchecked',
        disabledState: true,
        className: 'border-white-800 bg-white-50 !opacity-25'
      }
    ],
    defaultVariants: {
      variant: 'square',
      size: 'md',
      state: 'unchecked',
      disabledState: false
    }
  }
);

type CheckboxVariantProps = VariantProps<typeof checkboxVariants>;

export interface CheckboxProps extends CheckboxVariantProps, BaseCheckboxAttributes {
  checked?: boolean;
  disabled?: boolean;
  label?: string;
  description?: string;
  className?: string;
  onChange?: (checked: boolean) => void;
}

export const Checkbox = React.forwardRef<HTMLButtonElement, CheckboxProps>(
  (
    {
      checked,
      onChange,
      variant = 'square',
      size = 'sm',
      label,
      description,
      disabled,
      className,
      ...props
    },
    ref
  ) => {
    const [isChecked, setIsChecked] = React.useState<boolean>(checked ?? false);
    const state = isChecked ? 'checked' : 'unchecked';
    const disabledState = disabled ? true : false;

    return label ? (
      <div className="flex flex-row gap-2 items-center">
        <button
          ref={ref}
          type="button"
          disabled={disabled}
          onClick={() => {
            if (disabled) return;
            const nextChecked = !isChecked;
            setIsChecked(nextChecked);
            onChange?.(nextChecked);
          }}
          className={cn(checkboxVariants({ variant, size, state, disabledState }), className)}
          {...props}
        >
          {isChecked && variant === 'square' && (
            <Icon
              className={cn(
                size === 'sm' ? 'w-2.5 h-2.5' : 'w-3 h-3',
                'text-xl !text-primary-base'
              )}
              name="check"
            ></Icon>
          )}
          {isChecked && variant === 'radio' && (
            <div
              className={cn(
                size === 'sm' ? 'w-2 h-2' : 'w-2.5 h-2.5',
                ' rounded-full bg-primary-base'
              )}
            />
          )}
          {isChecked && variant === 'circle' && (
            <Icon
              className={cn(
                size === 'sm' ? 'w-2.5 h-2.5' : 'w-3 h-3',
                'stroke-[3] !text-text-white'
              )}
              name="check"
            ></Icon>
          )}
        </button>
        <div className={cn(disabled && '!opacity-25', 'flex flex-col flex-nowrap')}>
          <h2 className="text-md font-medium">{label}</h2>
          <p className="font-light text-md text-sm">{description}</p>
        </div>
      </div>
    ) : (
      <button
        ref={ref}
        type="button"
        disabled={disabled}
        onClick={() => {
          if (disabled) return;
          const nextChecked = !isChecked;
          setIsChecked(nextChecked);
          onChange?.(nextChecked);
        }}
        className={cn(checkboxVariants({ variant, size, state, disabledState }), className)}
        {...props}
      >
        {isChecked && variant === 'square' && (
          <Icon
            className={cn(size === 'sm' ? 'w-2.5 h-2.5' : 'w-3 h-3', 'text-xl !text-primary-base')}
            name="check"
          ></Icon>
        )}
        {isChecked && variant === 'radio' && (
          <div
            className={cn(
              size === 'sm' ? 'w-2 h-2' : 'w-2.5 h-2.5',
              ' rounded-full bg-primary-base'
            )}
          />
        )}
        {isChecked && variant === 'circle' && (
          <Icon
            className={cn(size === 'sm' ? 'w-2.5 h-2.5' : 'w-3 h-3', 'stroke-[3] !text-text-white')}
            name="check"
          ></Icon>
        )}
      </button>
    );
  }
);

Checkbox.displayName = 'Checkbox';
