import { Button, Icon, LoginForm, Logo } from '@/libs/ui/components';
import { BaseLayout } from '@/libs/ui/layouts';
import { useTranslation } from '@/libs/ui/provider';
import { useState } from 'react';
import login from '@/middleware/auth/login';
import { useNavigate } from 'react-router-dom';
import { AnimatePresence, motion } from 'framer-motion';

export default function Login() {
  const t = useTranslation('pages.login');
  const [serverError, setServerError] = useState<string | null>(null);
  const [loading, setLoading] = useState<boolean>(false);
  const navigate = useNavigate();

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
      <Button
        onClick={() => navigate('/')}
        outline={'primary'}
        size="md"
        className="absolute top-8 gap-2 left-8"
      >
        <Icon name="arrow-right"></Icon> {t('goBack')}
      </Button>
      <AnimatePresence>
        <motion.div
          initial={{ opacity: 0, y: -10 }}
          animate={{ opacity: 1, y: 0 }}
          exit={{ opacity: 0, y: -10 }}
          className="flex flex-col items-center"
          transition={{ duration: 0.6, ease: 'easeOut' }}
        >
          <Logo className="pb-4"></Logo>
          <LoginForm
            serverError={serverError}
            isSubmitting={loading}
            onSubmit={handleLogin}
          ></LoginForm>
        </motion.div>
      </AnimatePresence>
    </BaseLayout>
  );
}
