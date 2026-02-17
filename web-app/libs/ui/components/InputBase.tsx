import * as React from 'react';
import { cva, type VariantProps } from 'class-variance-authority';
import { cn } from '../utils';
import { GLOBAL_HEIGHT } from '../types/global-size';

type BaseInputAttributes = Pick<
  React.InputHTMLAttributes<HTMLInputElement>,
  | 'id'
  | 'name'
  | 'placeholder'
  | 'tabIndex'
  | 'autoFocus'
  | 'autoComplete'
  | 'readOnly'
  | 'required'
  | 'min'
  | 'max'
  | 'step'
  | 'pattern'
  | 'maxLength'
  | 'minLength'
  | 'onKeyDown'
  | 'onKeyUp'
  | 'onKeyPress'
  | 'onMouseDown'
  | 'onMouseUp'
  | 'onMouseEnter'
  | 'onMouseLeave'
  | 'onMouseOver'
  | 'onClick'
  | 'className'
  | 'disabled'
  | 'type'
  | 'value'
  | 'defaultValue'
  | 'onChange'
  | 'onBlur'
  | 'onFocus'
  | 'inputMode'
  | 'onInput'
>;

const inputVariants = cva(
  `bg-background-inputs w-full transition-all rounded-lg border flex flex-row flex-nowrap`,
  {
    variants: {
      hasError: {
        true: '',
        false: 'border-white-800 focus-within:!border-brand-blue-400',
      },
      variant: {
        default: '',
        website: '[&>div:first-child]:border-r-2 [&>*:first-child]:border-r-white-800',
        currency: '',
      },
    },
    compoundVariants: [
      // Default / Currency focus on wrapper
      {
        hasError: false,
        variant: ['default', 'currency'],
        className: 'focus-within:ring-4 focus-within:!ring-brand-blue-100',
      },
      {
        hasError: true,
        variant: ['default', 'currency'],
        className: 'border-error-text focus-within:ring-4 focus-within:!ring-error-bg',
      },
      // WebsiteField — no focus-within, the input itself will handle focus, also note that this variant is mostly for showcase purposes and isnt actively supported
      {
        hasError: false,
        variant: 'website',
        className:
          '[&>div:last-child:focus-within]:ring-4 [&>div:last-child:focus-within]:ring-brand-blue-100',
      },
      {
        hasError: true,
        variant: 'website',
        className:
          '[&>div:last-child:focus-within]:ring-4 [&>div:last-child:focus-within]:ring-error-bg border-white-800 [&>div:last-child]:border-error-text',
      },
    ],
    defaultVariants: {
      hasError: false,
      variant: 'default',
    },
  }
);

type InputVariants = VariantProps<typeof inputVariants>;

export interface InputProps extends BaseInputAttributes, InputVariants {
  leftContent?: React.ReactNode;
  rightContent?: React.ReactNode;
  leftContentClassName?: string;
  rightContentClassName?: string;
  height?: GLOBAL_HEIGHT;
}

export const InputBase = React.forwardRef<HTMLInputElement, InputProps>(
  (
    {
      className,
      leftContent,
      variant,
      hasError,
      rightContent,
      leftContentClassName,
      rightContentClassName,
      height = 'md',
      ...props
    },
    ref
  ) => {
    return (
      <div
        className={cn(
          inputVariants({ hasError, variant }),
          GLOBAL_HEIGHT[height],
          'p-2',
          className
        )}
        ref={ref}
      >
        {leftContent && (
          <div
            className={cn(
              leftContentClassName,
              'flex items-center justify-center text-text-grey ps-3 pr-2'
            )}
          >
            {leftContent}
          </div>
        )}
        <div className="transition-all w-[100%] rounded-r-lg flex flex-row flex-nowrap border-2 border-transparent">
          <input
            {...props}
            className="bg-transparent w-[100%] border-transparent placeholder:text-text-grey text-text-black font-light  focus:outline-none focus:border-transparent"
          ></input>
          {rightContent && (
            <div
              className={cn(
                rightContentClassName,
                'text-text-grey flex items-center justify-center flex-row flex-nowrap'
              )}
            >
              {rightContent}
            </div>
          )}
        </div>
      </div>
    );
  }
);

InputBase.displayName = 'InputBase';
