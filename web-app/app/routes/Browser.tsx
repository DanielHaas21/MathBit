import getApiConfig from '@/apiConfig';
import {
  BrowserItem,
  Button,
  Datepicker,
  Icon,
  InputField,
  InputWrapper,
  Label,
  useToast,
  WarningModal,
} from '@/libs/ui/components';
import { Header } from '@/libs/ui/components/Header';
import { BaseLayout, Paper } from '@/libs/ui/layouts';
import { useTranslation } from '@/libs/ui/provider/UiProvider';
import { RangeDate } from '@/libs/ui/types/RangeDate';
import { RootState } from '@/store/store';
import { AnimatePresence, motion } from 'framer-motion';
import { useEffect, useRef, useState } from 'react';
import { useSelector } from 'react-redux';
import { useNavigate } from 'react-router-dom';
import {
  deleteMathProblem,
  deleteMathProblemStepsByProblemId,
  getMathProblems,
  MathProblem,
} from 'web-api-client';

export default function Browser() {
  const user = useSelector((state: RootState) => state.User);
  const navigate = useNavigate();

  const [problems, setProblems] = useState<MathProblem[]>([]);
  const [searchInput, setSearchInput] = useState<string>('');
  const [searchFilter, setSearchFilter] = useState<string>('');
  const [date, setDate] = useState<RangeDate>({});
  // Filter states
  const debounceTimeout = useRef<NodeJS.Timeout | null>(null); // for debouncing search input
  const divRef = useRef<HTMLDivElement | null>(null); // ref to the scrollable div
  const [page, setPage] = useState<number>(0); // pagination state
  const pageSize = 20; // fixed limit
  // to avoid duplicate fetches
  const lastQueryRef = useRef<{
    searchFilter: string;
    page: number;
    date: RangeDate;
  } | null>(null);

  // modal states and utils
  const [pendingId, setPendingId] = useState<number | null>(null);
  const [loading, setLoading] = useState<boolean>(true);
  const [isOpen, setIsOpen] = useState<boolean>(false);

  const { show } = useToast();

  const t = useTranslation('pages.browser');

  // Infinite scroll handler
  const handleInfiniteScroll = (
    e: React.UIEvent<HTMLElement, UIEvent>,
    loading: boolean,
    callback: () => void
  ) => {
    const { scrollTop, scrollHeight, clientHeight } = e.currentTarget; // Get scroll metrics

    if (loading || scrollHeight === clientHeight) return; // if already loading or no scrollable content, exit
    const scrollPercent = (scrollTop / (scrollHeight - clientHeight)) * 100; // Calculate scroll percentage

    if (scrollPercent > 80) {
      // If scrolled more than 80%, trigger loading more content
      callback();
    }
  };

  // debounce search input
  useEffect(() => {
    if (debounceTimeout.current) clearTimeout(debounceTimeout.current);

    // when timeout completes, set the search filter and reset to first page
    debounceTimeout.current = setTimeout(() => {
      setSearchFilter(searchInput);
      setPage(0);
      if (divRef.current) {
        divRef.current.scrollTo({ top: 0, behavior: 'smooth' });
      }
    }, 500);
  }, [searchInput]);

  // fetch problems when filter or page changes
  useEffect(() => {
    if (
      (user.authStatus === 'unknown' && !user.user?.id) || //this is the initial state while we check auth status
      (user.authStatus === 'authenticated' && !user.user?.id) // accessToken refresh before the user, hence why also this state happens
    )
      return;

    // if me and refresh fails the state will be unauthenticated therefore redirect to login
    if (user.authStatus === 'unauthenticated') {
      navigate('/login');
      show({
        icon: 'triangle-exclamation',
        variant: 'error',
        title: t('messages.notAuthenticated'),
        description: t('messages.notAuthenticatedDescription'),
      });
      return;
    }

    // this is a query comparator to avoid duplicate fetches
    const currentQuery = {
      searchFilter,
      page,
      date,
    };

    // on each query change, compare with last query and if same, skip fetch
    if (
      lastQueryRef.current &&
      lastQueryRef.current.searchFilter === currentQuery.searchFilter &&
      lastQueryRef.current.page === currentQuery.page &&
      JSON.stringify(lastQueryRef.current.date) === JSON.stringify(currentQuery.date)
    ) {
      return;
    }

    lastQueryRef.current = currentQuery; // update last query ref

    setLoading(true);
    const fetch = async () => {
      try {
        const response = await getMathProblems(
          {
            offset: page * pageSize,
            limit: pageSize,
          },
          {
            userId: user.user?.id as number,
            name: searchFilter,
            dateFrom: date.from,
            dateTo: date.to,
          },
          getApiConfig()
        );

        // Append results for subsequent pages; replace only on first page
        setProblems((prev) => (page === 0 ? response.data : [...prev, ...response.data]));
      } catch (err: any) {
      } finally {
        setLoading(false);
      }
    };
    fetch();
  }, [searchFilter, page, user, date]);

  return (
    <>
      <WarningModal
        Open={isOpen}
        onResolve={async (result) => {
          setIsOpen(false);

          if (result) {
            setProblems((prev) => prev.filter((p) => p.id !== pendingId));
            await deleteMathProblem(pendingId as number, getApiConfig());
            await deleteMathProblemStepsByProblemId(pendingId as number, getApiConfig());
            setPendingId(null);
          }
        }}
      ></WarningModal>
      <BaseLayout>
        <BaseLayout.Menu>
          <Header route={[{ locKey: 'browser', pageRoute: '/browser' }]} />
        </BaseLayout.Menu>
        <BaseLayout.Content className="flex flex-col items-center p-6">
          <AnimatePresence>
            <motion.div
              initial={{ opacity: 0, y: -10 }}
              animate={{ opacity: 1, y: 0 }}
              exit={{ opacity: 0, y: -10 }}
              className="w-full"
              transition={{ duration: 0.6, ease: 'easeOut' }}
            >
              <Paper className="mb-8  mt-5 w-full h-fit !p-[10px] !pb-[16px] bg-white-50 border border-white-800 rounded-xl">
                <Paper.Content className="grid grid-cols-1 md:grid-cols-2 place-items-end px-6 pb-2">
                  <div className="flex flex-col md:flex-row w-full gap-4">
                    <div className="w-full xl:w-[40%] ">
                      <InputField
                        required={false}
                        className=" w-full bg-white-50"
                        label={t('filters.search')}
                        placeholder={t('filters.searchPlaceholder')}
                        onChange={(e) => setSearchInput(e.target.value)}
                        type="text"
                        value={searchInput}
                      />
                    </div>
                    <div className="w-full xl:w-[300px] relative">
                      <InputWrapper label={t('filters.date')} required={false}>
                        <Datepicker
                          className="bg-white-50"
                          mode="range"
                          value={date}
                          placeholder={t('filters.datePlaceholder')}
                          onChange={(newDate) => setDate(newDate as RangeDate)}
                        ></Datepicker>
                      </InputWrapper>
                    </div>
                    <Button
                      outline={'primary'}
                      onClick={() => {
                        setSearchInput('');
                        setDate({});
                      }}
                      className="gap-2 w-full  md:w-fit h-[46px] md:mr-4 mt-[28px]"
                    >
                      <Icon name="arrows-rotate" />
                      {t('filters.reset')}
                    </Button>
                  </div>

                  <Button
                    onClick={() => {
                      navigate('/browser/editor');
                    }}
                    className="gap-2 w-full md:w-fit h-[46px] md:mr-4 mt-[28px] self-end"
                  >
                    <Icon name="gear" />
                    {t('editor')}
                  </Button>
                </Paper.Content>
              </Paper>
            </motion.div>
          </AnimatePresence>
          <AnimatePresence>
            <motion.div
              initial={{ opacity: 0, y: -10 }}
              animate={{ opacity: 1, y: 0 }}
              exit={{ opacity: 0, y: -10 }}
              className="w-full"
              transition={{ duration: 0.6, ease: 'easeOut' }}
            >
              <AnimatePresence>
                <motion.div
                  initial={{ opacity: 0, y: -10 }}
                  animate={{ opacity: 1, y: 0 }}
                  exit={{ opacity: 0, y: -10 }}
                  ref={divRef}
                  className="m-4 w-full !max-h-[1100px] md:!max-h-[750px] overflow-y-scroll grid grid-cols-1 md:grid-cols-2 xl:grid-cols-3 gap-4"
                  onScroll={(e) =>
                    handleInfiniteScroll(e, loading, () => setPage((prev) => prev + 1))
                  }
                  transition={{ duration: 0.6, ease: 'easeOut' }}
                >
                  {problems.map((problem, index) => (
                    <BrowserItem
                      key={index}
                      id={problem.id}
                      name={problem.name}
                      description={problem.description}
                      created={problem.created}
                      updated={problem.updated}
                      onDelete={(id) => {
                        setIsOpen(true);
                        setPendingId(id);
                      }}
                      onView={(id) => {
                        navigate(`/browser/editor/${id}`);
                      }}
                    />
                  ))}
                </motion.div>
              </AnimatePresence>

              {problems.length === 0 && (
                <div className="flex flex-col items-center gap-2 mt-[100px]">
                  <Icon name="magnifying-glass" size="xl" className="text-text-grey"></Icon>

                  <Label className="text-text-black" size="lg">
                    {t('emptyResult.all')}
                  </Label>
                  <Label className="text-text-grey text-center" size="md">
                    {t('emptyResult.all2')}
                  </Label>
                </div>
              )}
            </motion.div>
          </AnimatePresence>
        </BaseLayout.Content>
      </BaseLayout>
    </>
  );
}
