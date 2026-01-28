import { Label, LoginForm, Logo, MathField } from '@/libs/ui/components';
import { BaseLayout } from '@/libs/ui/layouts';
import logo from '../../libs/ui/assets/images/dark/logo_medium.svg';
import { useTranslation } from '@/libs/ui/provider';
import { useState } from 'react';
import login from '@/middleware/auth/login';
import { useNavigate } from 'react-router-dom';
import signup from '@/middleware/auth/signup';
import { SignupForm } from '@/libs/ui/components/SignupForn';

export default function Signup() {
  const t = useTranslation('pages.signup');
  const [serverError, setServerError] = useState<string | null>(null);
  const [loading, setLoading] = useState(false);
  const navigate = useNavigate();

  const handleSignup = async (username: string, email: string, password: string) => {
    setLoading(true);
    setServerError(null);
    const result = await signup({ username, email, password });

    setLoading(false);

    if (result.ok === true) {
      navigate('/browser');
      return;
    }

    if (result?.errorCode) {
      setServerError(t(`errors.${result.errorCode}`));
    } else {
      setServerError(t('errors.generic'));
    }
  };
  return (
    <BaseLayout className="justify-center items-center">
      <Logo className="pb-4"></Logo>
      <SignupForm
        serverError={serverError}
        isSubmitting={loading}
        onSubmit={handleSignup}
      ></SignupForm>
    </BaseLayout>
  );
}
