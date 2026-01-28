import * as React from 'react';
import { Paper } from '../layouts';
import { Button } from './Button';
import { Icon } from './Icon';
import { InputBase } from './InputBase';
import { InputWrapper } from './InputWrapper';
import { useTranslation } from '../provider';
import { Label } from './Label';

export interface SignupFormProps {
  onSubmit: (username: string, email: string, password: string) => void;
  serverError?: string | null;
  isSubmitting?: boolean;
}

export function SignupForm(props: SignupFormProps) {
  const t = useTranslation('ui.signupForm');
  const { onSubmit, serverError, isSubmitting } = props;

  const [username, setUsername] = React.useState<string>('');
  const [email, setEmail] = React.useState<string>('');
  const [password, setPassword] = React.useState<string>('');
  const [confirmPassword, setConfirmPassword] = React.useState<string>('');

  const [emailError, setEmailError] = React.useState<string | null>(null);
  const [passwordError, setPasswordError] = React.useState<string | null>(null);
  const [confirmPasswordError, setConfirmPasswordError] = React.useState<string | null>(null);
  const [usernameError, setUsernameError] = React.useState<string | null>(null);

  const [submitted, setSubmitted] = React.useState<boolean>(false);
  const isValidEmail = (value: string) => {
    // Simple RFC5322-like email check suitable for client validation
    const emailRegex = /^[^\s@]+@[^\s@]+\.[^\s@]+$/;
    return emailRegex.test(value);
  };

  const validateUsername = (value: string) => {
    if (!value.trim()) {
      return 'Username is required';
    }
    return null;
  };

  const validateEmail = (value: string) => {
    if (!value.trim()) {
      return 'Email is required';
    }
    if (!isValidEmail(value.trim())) {
      return 'Please enter a valid email address';
    }
    return null;
  };

  const validatePassword = (value: string) => {
    if (!value.trim()) {
      return 'Password is required';
    }

    if (value.trim().length < 8) {
      return 'Password must be at least 8 characters long';
    }

    if (password.trim() !== confirmPassword.trim()) {
      return 'Passwords do not match';
    }
    return null;
  };

  const handleSubmit = () => {
    const eErr = validateEmail(email);
    const pErr = validatePassword(password);
    const uErr = validateUsername(username);

    setUsernameError(uErr);
    setEmailError(eErr);
    setPasswordError(pErr);
    setSubmitted(true);

    if (eErr || pErr) {
      return;
    }

    onSubmit(username, email, password);
  };
  return (
    <div className="flex flex-col items-center gap-8">
      <Paper className="flex flex-col items-center gap-2 shadow-lg border border-white-800 rounded-xl !min-w-[480px]  p-10">
        <Paper.Content className="w-full flex flex-col items-center gap-4">
          <InputWrapper
            label={t('email.label')}
            className="w-full"
            required={false}
            isError={submitted && !!emailError}
            errorText={submitted ? emailError : null}
          >
            <InputBase
              leftContent={<Icon name="user"></Icon>}
              type="email"
              className="bg-white-50"
              placeholder={t('email.placeholder')}
              name="email"
              id="login-email"
              value={email}
              onChange={(e) => {
                setEmail(e.target.value);
                if (submitted) setSubmitted(false);
              }}
            ></InputBase>
          </InputWrapper>

          <InputWrapper
            label={t('username.label')}
            className="w-full"
            required={false}
            isError={submitted && !!usernameError}
            errorText={submitted ? usernameError : null}
          >
            <InputBase
              leftContent={<Icon name="user"></Icon>}
              type="text"
              className="bg-white-50"
              placeholder={t('username.placeholder')}
              name="username"
              id="signup-username"
              value={username}
              onChange={(e) => {
                setUsername(e.target.value);
                if (submitted) setSubmitted(false);
              }}
            ></InputBase>
          </InputWrapper>

          <InputWrapper
            label={t('password.label')}
            className="w-full"
            required={false}
            isError={(submitted && !!passwordError) || !!serverError}
            errorText={submitted ? passwordError : null}
          >
            <InputBase
              leftContent={<Icon name="lock"></Icon>}
              placeholder={t('password.placeholder')}
              type="password"
              className="bg-white-50"
              name="password"
              id="login-password"
              value={password}
              onChange={(e) => {
                setPassword(e.target.value);
                if (submitted) setSubmitted(false);
              }}
            ></InputBase>
          </InputWrapper>

          <InputWrapper
            label={t('confirmPassword.label')}
            className="w-full"
            required={false}
            isError={(submitted && !!passwordError) || !!serverError}
            errorText={submitted ? passwordError : null}
          >
            <InputBase
              leftContent={<Icon name="lock"></Icon>}
              placeholder={t('confirmPassword.placeholder')}
              type="password"
              className="bg-white-50"
              name="confirmPassword"
              id="signup-confirm-password"
              value={confirmPassword}
              onChange={(e) => {
                setConfirmPassword(e.target.value);
                if (submitted) setSubmitted(false);
              }}
            ></InputBase>
          </InputWrapper>

          <Button
            size="full"
            className="self-center px-4 !w-full mt-[20px]"
            onClick={() => handleSubmit()}
          >
            {t('submit')}
          </Button>
          {serverError && <Label className="text-error-text text-center mt-2">{serverError}</Label>}
        </Paper.Content>
      </Paper>
    </div>
  );
}
