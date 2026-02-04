import * as React from 'react';
import { Paper } from '../layouts';
import { Button } from './Button';
import { Icon } from './Icon';
import { InputBase } from './InputBase';
import { InputWrapper } from './InputWrapper';
import { useTranslation } from '../provider';
import { Label } from './Label';
import { Link } from 'react-router-dom';

export interface LoginFormProps {
  onSubmit: (email: string, password: string) => void;
  serverError?: string | null;
  isSubmitting?: boolean;
}

export function LoginForm(props: LoginFormProps) {
  const t = useTranslation('ui.loginForm');
  const { onSubmit, serverError, isSubmitting } = props;

  const [email, setEmail] = React.useState<string>('');
  const [password, setPassword] = React.useState<string>('');

  const [emailError, setEmailError] = React.useState<string | null>(null);
  const [passwordError, setPasswordError] = React.useState<string | null>(null);
  const [submitted, setSubmitted] = React.useState<boolean>(false);
  const isValidEmail = (value: string) => {
    // Simple RFC5322-like email check suitable for client validation
    const emailRegex = /^[^\s@]+@[^\s@]+\.[^\s@]+$/;
    return emailRegex.test(value);
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
    return null;
  };

  const handleSubmit = () => {
    const eErr = validateEmail(email);
    const pErr = validatePassword(password);

    setEmailError(eErr);
    setPasswordError(pErr);
    setSubmitted(true);

    if (eErr || pErr) {
      return;
    }

    onSubmit(email, password);
  };
  return (
    <div className="flex flex-col items-center gap-8">
      <Paper className="flex flex-col items-center gap-2 shadow-lg border border-white-800 rounded-xl w-[90vw] md:w-[480px]  p-10">
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

          <Button
            size="full"
            className="self-center px-4 !w-full mt-[20px]"
            onClick={() => handleSubmit()}
          >
            {t('submit')}
          </Button>
          {serverError && <Label className="text-error-text text-center mt-2">{serverError}</Label>}
          <Label className="text-sm  mt-4">
            {t('noAccount')}
            <Link
              to="/signup"
              className="text-primary-base hover:text-primary-hover hover:underline"
            >
              {t('registerLink')}
            </Link>
          </Label>
        </Paper.Content>
      </Paper>
    </div>
  );
}
