import { Label, LoginForm, MathField } from '@/libs/ui/components';
import { BaseLayout } from '@/libs/ui/layouts';
import logo from '../../libs/ui/assets/images/dark/logo_medium.svg';
import { useTranslation } from '@/libs/ui/provider';
import { useState } from 'react';
import login from '@/middleware/auth/login';
import { useNavigate } from 'react-router-dom';

export default function Login() {
  const t = useTranslation('pages.login');
  const [serverError, setServerError] = useState<string | null>(null);
  const [loading, setLoading] = useState(false);
  const navigate = useNavigate();
  // useEffect(() => {
  //   async function d() {
  //     const auth = await login({ email: 'e.procházková4@example.com', password: 'test' });
  //   }

  //   d();
  // }, []);

  const handleLogin = async (email: string, password: string) => {
    setLoading(true);
    setServerError(null);
    const result = await login({ email, password });

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
      <div className="flex flex-row pt-5 pb-4">
        <Label size="lg" className="text-[46px] font-bold select-none ps-4">
          Mathbit
        </Label>
        <img src={logo} alt="Mathbit Logo" className="ps-2 " />
      </div>
      <LoginForm
        serverError={serverError}
        isSubmitting={loading}
        onSubmit={handleLogin}
      ></LoginForm>
    </BaseLayout>
  );
}
