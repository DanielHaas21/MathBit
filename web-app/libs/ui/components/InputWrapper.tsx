import * as React from 'react';
import { cn } from '../utils';
import { cva, type VariantProps } from 'class-variance-authority';
import { Label } from './Label';

const inputWrapperVariants = cva(
  `
    flex flex-col
  `,
  {
    variants: {
      LabelSize: {
        xs: '',
        sm: '',
        md: '',
        lg: '',
        xl: ''
      }
    },
    defaultVariants: {
      LabelSize: 'md'
    }
  }
);

type InputWrapperVariants = VariantProps<typeof inputWrapperVariants>;

export interface InputWrapperProps extends InputWrapperVariants {
  className?: string;
  children?: React.ReactNode;
  htmlFor?: string;
  label?: string;
  isError?: boolean;
  errorText?: string | null;
  hint?: string;
  required?: boolean;
}

export function InputWrapper(props: InputWrapperProps) {
  const {
    className,
    LabelSize,
    label,
    htmlFor,
    children,
    hint,
    isError,
    errorText,
    required = true
  } = props;

  return (
    <div className={cn(inputWrapperVariants({ className, LabelSize }), 'group')}>
      {label && (
        <Label size={LabelSize} className="flex gap-2" htmlFor={htmlFor}>
          {label}
          {required && <span className="text-error-text">*</span>}
        </Label>
      )}
      {/* Clone children and apply error class to inputs */}
      {React.Children.map(children, (child) => {
        if (React.isValidElement(child)) {
          const existingClassName = (child.props as { className?: string }).className || '';
          const extraProps = {
            className: cn(existingClassName)
          };

          // Only pass hasError to custom components
          if (typeof child.type !== 'string') {
            (extraProps as any).hasError = isError;
          }

          return React.cloneElement(child, extraProps);
        }
        return child;
      })}

      {/* Message under input */}
      {errorText && isError ? (
        <p className="text-sm text-error-text  transition-all">{errorText}</p>
      ) : hint ? (
        <p className="text-sm text-text-grey text-muted-foreground">{hint}</p>
      ) : null}
    </div>
  );
}

InputWrapper.displayName = 'InputWrapper';
