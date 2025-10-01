import React from 'react';
import { InputWrapper, type InputWrapperProps } from '../InputWrapper';
import { InputBase, type InputProps } from '../InputBase';

export interface InputFieldProps extends InputProps, Omit<InputWrapperProps, 'children'> {}

export const InputField = React.forwardRef<HTMLInputElement, InputFieldProps>((props, ref) => {
  const { LabelSize, htmlFor, isError, label, errorText, hint, required, ...inputProps } = props;

  return (
    <InputWrapper
      LabelSize={LabelSize}
      label={label ?? 'Input'}
      hint={hint}
      isError={isError}
      errorText={errorText}
      htmlFor={htmlFor}
      required={required}
    >
      <InputBase id={htmlFor} ref={ref} type="text" {...inputProps} />
    </InputWrapper>
  );
});

InputField.displayName = 'InputField';
