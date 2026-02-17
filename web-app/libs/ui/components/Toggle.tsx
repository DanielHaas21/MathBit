import { cva, type VariantProps } from 'class-variance-authority';
import { cn } from '../utils';
import { Field, Label, Switch } from '@headlessui/react';
import * as React from 'react';

const ToggleVariants = cva(
  `
  relative inline-flex h-6 w-11 items-center rounded-full transition-colors duration-200 focus:outline-none border-2 border-transparent
  hover:border-primary-hover/50 `,
  {
    variants: {
      size: {
        sm: 'w-[36px] h-[20px]',
        md: 'w-[44px] h-[24px]',
      },
      variant: {
        defaultBackground: '',
        light: '',
        neutral: '',
        default: '',
        background: '',
      },
      IsEnabled: {
        true: '',
        false: '',
      },
    },
    compoundVariants: [
      //DefaultBackground on-off
      {
        IsEnabled: false,
        variant: 'defaultBackground',
        className: 'bg-white-50 [&>span]:shadow-[0px_1px_5px_1px_theme(colors.black.100)]',
      },
      {
        IsEnabled: true,
        variant: 'defaultBackground',
        className: 'bg-primary-base [&>span]:shadow-[0px_1px_5px_1px_theme(colors.black.600)]',
      },
      //Default on-off
      {
        IsEnabled: false,
        variant: 'default',
        className: 'bg-white-400  [&>span]:shadow-[0px_1px_5px_1px_theme(colors.black.100)]',
      },
      {
        IsEnabled: true,
        variant: 'default',
        className: 'bg-primary-base [&>span]:shadow-[0px_1px_5px_1px_theme(colors.black.600)]',
      },
      //background on-off
      {
        IsEnabled: false,
        variant: 'background',
        className: 'bg-white-50  [&>span]:shadow-[0px_1px_5px_1px_theme(colors.black.100)]',
      },
      {
        IsEnabled: true,
        variant: 'background',
        className: 'bg-white-50 [&>span]:shadow-[0px_1px_5px_1px_theme(colors.black.100)]',
      },
      //outline
      {
        IsEnabled: false,
        variant: 'light',
        className: 'bg-brand-blue-50  [&>span]:shadow-[0px_1px_5px_1px_theme(colors.black.100)]',
      },
      {
        IsEnabled: true,
        variant: 'light',
        className: 'bg-brand-blue-100 [&>span]:shadow-[0px_1px_5px_1px_theme(colors.black.200)]',
      },
    ],
    defaultVariants: {
      variant: 'default',
      IsEnabled: false,
      size: 'md',
    },
  }
);

type ToggleVariantProps = VariantProps<typeof ToggleVariants>;

export interface ToggleProps extends ToggleVariantProps {
  labelText?: string;
  className?: string;
  value?: boolean;
  disabled?: boolean;
  onChange?: (val: boolean) => void;
}

export const Toggle = React.forwardRef<HTMLElement, ToggleProps>(
  ({ variant, size = 'md', labelText, IsEnabled, onChange, value, disabled, ...props }, ref) => {
    const [enabled, setEnabled] = React.useState<boolean>(value ?? false);

    const thumbTranslate = {
      sm: enabled ? 'translate-x-[17px]' : 'translate-x-[2px]',
      md: enabled ? 'translate-x-[18px]' : 'translate-x-1',
    };
    const thumbSize = {
      sm: 'h-[14px] w-[14px]',
      md: 'h-[18px] w-[18px]',
    };

    return labelText ? (
      <Field className="inline-flex gap-2">
        <Switch
          disabled={disabled}
          checked={enabled}
          onChange={() => {
            const next = !enabled;
            setEnabled(next);
            onChange?.(next);
          }}
          className={cn(
            ToggleVariants({ variant, size, IsEnabled: enabled }),
            disabled && 'opacity-50 hover:!border-transparent'
          )}
          {...props}
        >
          <span
            className={cn(
              thumbTranslate[size!],
              thumbSize[size!],
              ' inline-block transform rounded-full bg-white-100 transition-transform'
            )}
          ></span>
        </Switch>
        <Label>{labelText}</Label>
      </Field>
    ) : (
      <Switch
        checked={enabled}
        disabled={disabled}
        onChange={() => {
          const next = !enabled;
          setEnabled(next);
          onChange?.(next);
        }}
        className={cn(
          ToggleVariants({ variant, size, IsEnabled: enabled }),
          disabled && 'opacity-50 hover:!border-transparent'
        )}
        {...props}
      >
        <span
          className={cn(
            thumbTranslate[size!],
            thumbSize[size!],
            ' inline-block transform rounded-full bg-white-100 transition-transform'
          )}
        ></span>
      </Switch>
    );
  }
);

Toggle.displayName = 'Toggle';
