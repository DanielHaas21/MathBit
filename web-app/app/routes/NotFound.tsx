import { Button, Icon, Label, Logo } from '@/libs/ui/components';
import { BaseLayout} from '@/libs/ui/layouts';
import { useNavigate } from 'react-router-dom';
import { useTranslation } from '@/libs/ui/provider/UiProvider';
export function NotFound() {
  const navigate = useNavigate();
  const t = useTranslation('pages.notfound');
  
  return (
    <BaseLayout className="">
      <Logo></Logo>
      <div className="w-full flex h-full flex-col items-center justify-center mt-[-50px] ">
        <Icon name="triangle-exclamation" className="text-black-500" fontSize={120}></Icon>
        <Label size={'xl'} className="text-black-700 mt-4">
          {t('title')}
        </Label>
        <Label size={'lg'} className="text-black-700 mt-4">
          {t('description')}
        </Label>
        <Button outline={'neutral'} className="gap-4 mt-3" onClick={() => navigate('/')}>
          <Icon name="arrows-rotate"></Icon> {t('returnHome')}
        </Button>
      </div>
    </BaseLayout>
  );
}
