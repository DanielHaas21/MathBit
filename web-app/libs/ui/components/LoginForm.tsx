'use client';

import { Paper } from '../layouts';
import { FieldDefinition, LinkItem } from '../types';
import { Button } from './Button';
import { Icon } from './Icon';
import { InputField } from './Inputfields';

export interface LoginProps {
  title: React.ReactNode;
  buttonTitle: string;
  forgotPasswordLink?: LinkItem;
  links?: boolean;
  registrationLink?: LinkItem;
  usernameField: FieldDefinition;
  passwordField: FieldDefinition;
  onSubmit?: (username: string, password: string) => void;
}

// TO BE REFACTORED

export function Login(props: LoginProps) {
  const {
    title,
    forgotPasswordLink,
    registrationLink,
    buttonTitle,
    usernameField,
    passwordField,
    links = false,
  } = props;
  const handleSubmit = () => {
    const username = (document.querySelector('input[name="username"]') as HTMLInputElement).value;
    const password = (document.querySelector('input[name="password"]') as HTMLInputElement).value;
    if (props.onSubmit) {
      props.onSubmit(username, password);
    } else {
      console.warn('onSubmit handler is not provided');
    }
  };
  return (
    <div className="flex flex-col items-center gap-8">
      logo here
      <Paper className="flex flex-col items-center gap-2 shadow-lg border border-white-800 rounded-xl !min-w-[380px]  p-10">
        <Paper.Title>{title}</Paper.Title>
        <Paper.Content className="w-full flex flex-col items-center gap-4">
          <div className="w-full">
            <InputField
              leftContent={<Icon name="user"></Icon>}
              className="bg-white-50"
              height="sm"
              placeholder={usernameField.placeholder}
              label={usernameField.label}
              required={false}
              name="username"
            />
          </div>
          <div className="w-full">
            <InputField
              leftContent={<Icon name="bag-shopping"></Icon>}
              className=" bg-white-50"
              height="sm"
              placeholder={passwordField.placeholder}
              label={passwordField.label}
              name="password"
              type="password"
              required={false}
            />
          </div>

          <Button
            size="full"
            className="self-center px-4 !w-full mt-[20px]"
            onClick={() => handleSubmit()}
          >
            {buttonTitle}
          </Button>
          {links && (
            <footer className="flex justify-between gap-3">
              {forgotPasswordLink && (
                <a href={forgotPasswordLink.href}>{forgotPasswordLink.label}</a>
              )}
              {registrationLink && (
                <a href={registrationLink.href}>
                  {registrationLink.label}
                  <Icon name="chevron-down" rotate={270} />
                </a>
              )}
            </footer>
          )}
        </Paper.Content>
      </Paper>
    </div>
  );
}
